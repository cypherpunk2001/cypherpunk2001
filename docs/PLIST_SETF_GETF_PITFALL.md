# The `setf getf` Pitfall: Why Object Sync Broke

This document explains a subtle Common Lisp bug that caused hours of debugging when implementing object pickup and respawn sync in client/server mode.

## The Symptom

Arrows on the ground couldn't be picked up in client/server mode, and when finally fixed, they wouldn't visually respawn after the 5-second timer. The server logs showed everything working correctly, but clients never saw the updates.

## The Root Cause: `setf getf` Doesn't Add Keys

In Common Lisp, `setf getf` **only modifies existing keys** - it does NOT add new keys to a plist.

```lisp
;; This works - key exists
(let ((plist '(:name "arrow" :count 5)))
  (setf (getf plist :count) 10)
  plist)
;; => (:name "arrow" :count 10)

;; THIS SILENTLY FAILS - key doesn't exist
(let ((plist '(:name "arrow")))
  (setf (getf plist :respawn) 5.0)
  plist)
;; => (:name "arrow")  ;; :respawn was NOT added!
```

**The modification is silently ignored with no error or warning.**

## How This Bit Us

Zone objects are loaded from data files like this:

```lisp
;; data/zones/zone-1.lisp
(:objects ((:id :arrows :x 34 :y 32)))
```

The plist only has `:id`, `:x`, `:y`. When we tried to set `:respawn` and `:snapshot-dirty`:

```lisp
;; In pickup code - SILENTLY FAILS
(setf (getf object :respawn) 5.0)      ;; Key doesn't exist!
(setf (getf object :count) 0)          ;; Key doesn't exist!

;; In respawn code - SILENTLY FAILS
(setf (getf object :snapshot-dirty) t) ;; Key doesn't exist!
```

The server thought it was updating values. The values never changed. Clients never got updates.

## The Fix

Initialize ALL keys that might be modified when loading zone objects:

```lisp
;; In zone.lisp load-zone
(objects (mapcar (lambda (obj)
                   (list :id (getf obj :id)
                         :x (getf obj :x)
                         :y (getf obj :y)
                         :count (getf obj :count nil)      ;; Initialize!
                         :respawn 0.0                      ;; Initialize!
                         :respawnable (getf obj :respawnable t)
                         :snapshot-dirty nil))             ;; Initialize!
                 raw-objects))
```

## Prevention Rules

### Rule 1: Always Initialize Mutable Plist Keys

When creating plists that will be modified later, explicitly include ALL keys:

```lisp
;; BAD - missing keys that will be setf'd later
(list :id id :x x :y y)

;; GOOD - all mutable keys present
(list :id id :x x :y y :count nil :respawn 0.0 :dirty nil)
```

### Rule 2: Use Structs for Complex Mutable State

Structs don't have this problem. Prefer structs over plists for entities with mutable state:

```lisp
;; Structs work correctly
(defstruct zone-object id x y count respawn dirty)
(setf (zone-object-respawn obj) 5.0)  ;; Always works
```

We use plists for zone objects because they're data-driven and loaded from files, but this comes with the `setf getf` pitfall.

### Rule 3: Add Assertions During Development

When debugging sync issues, add assertions to catch missing keys:

```lisp
(defun set-object-respawn (object value)
  (assert (member :respawn object) ()
          "Object missing :respawn key - was it initialized?")
  (setf (getf object :respawn) value))
```

### Rule 4: Document Required Keys

When adding new plist-based features, document which keys must be initialized:

```lisp
;; Zone object required keys:
;; :id - symbol, object archetype ID
;; :x, :y - integers, tile coordinates
;; :count - integer or nil, items available (MUTABLE)
;; :respawn - float, seconds until respawn (MUTABLE)
;; :respawnable - boolean, can this respawn?
;; :snapshot-dirty - boolean, needs sync to clients (MUTABLE)
```

## Debugging Checklist

When object/entity sync breaks mysteriously:

1. **Check if the plist key exists** before `setf getf`
2. **Add logging BEFORE and AFTER** the setf to see if value changed
3. **Check initialization code** - is the key being created?
4. **Search for all `setf (getf` calls** on that plist type
5. **Verify the key name matches** (typos like `:respawn` vs `:respawn-timer`)

## Files Affected by This Fix

- `src/zone.lisp` - Initialize object plists with all required keys
- `src/progression.lisp` - Object pickup and respawn logic
- `src/save.lisp` - Delta snapshot serialization with dirty flag
- `src/net.lisp` - Client-side intent clearing after pickup

## Related: The Multi-Layer Bug

This bug was actually multiple issues stacked:

1. **Inventory not syncing** - Fixed by adding inventory to compact snapshots
2. **Objects not in delta snapshots** - Fixed by adding object serialization
3. **Player not marked dirty after pickup** - Fixed by setting snapshot-dirty
4. **`:respawn` key missing** - Fixed by initializing in zone.lisp
5. **`:snapshot-dirty` key missing** - Fixed by initializing in zone.lisp
6. **Respawn not syncing** - Fixed by checking dirty flag in delta serialization

Each layer masked the next. Only after fixing all of them did pickup and respawn work correctly.

## Summary

**The lesson:** In Common Lisp, `setf getf` silently fails if the key doesn't exist. Always initialize all mutable keys when creating plists, or use structs instead.
