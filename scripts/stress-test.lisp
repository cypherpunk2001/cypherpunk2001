;;;; scripts/stress-test.lisp
;;;; Simple headless stress test ("ask nicely")
;;;;
;;;; Behavior:
;;;; - Spawns 1 registering client every 0.5s, up to 2000 total.
;;;; - No auth retries. If auth fails or times out, the client is dropped.
;;;; - Stays at 2000 active clients until terminated (SIGTERM).
;;;;
;;;; Environment:
;;;;   MMORPG_HOST=127.0.0.1  - Server IP/host (only knob)

(defun load-quicklisp ()
  (let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (unless (probe-file setup)
      (format *error-output* "Quicklisp not found~%")
      (sb-ext:exit :code 1))
    (load setup)))

(defvar *run-id* (get-universal-time))
(defvar *client-id* 0)

(defvar *mmorpg-make-net-buffer* nil)
(defvar *mmorpg-make-intent* nil)
(defvar *mmorpg-send-auth* nil)
(defvar *mmorpg-send-net* nil)
(defvar *mmorpg-receive-net* nil)
(defvar *mmorpg-intent->plist* nil)
(defvar *mmorpg-set-dx* nil)
(defvar *mmorpg-set-dy* nil)
(defvar *usocket-connect* nil)
(defvar *usocket-close* nil)
(defvar *ql-register* nil)
(defvar *ql-quickload* nil)

(defvar *auth-ok-count* 0)
(defvar *auth-fail-count* 0)
(defvar *auth-timeout-count* 0)
(defvar *auth-drop-count* 0)

(defparameter *target* 2000)
(defparameter *spawn-interval* 0.5f0)
(defparameter *send-hz* 5.0f0)
(defparameter *tick* 0.05f0)
(defparameter *dir-interval* 3.0f0)
(defparameter *drain-max* 5)
(defparameter *auth-timeout* 30.0f0)

(defparameter *dir-dx*
  #(-1.0f0 1.0f0 0.0f0 0.0f0 -0.7f0 0.7f0 -0.7f0 0.7f0 0.0f0))
(defparameter *dir-dy*
  #(0.0f0 0.0f0 -1.0f0 1.0f0 -0.7f0 -0.7f0 0.7f0 0.7f0 0.0f0))

(defstruct (stress-client (:constructor %make-stress-client))
  id username socket buffer intent authenticated-p
  last-auth-time
  next-send-time next-dir-change)

(defun now-seconds ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun next-id ()
  (incf *client-id*))

(defun random-direction ()
  (let ((i (random (length *dir-dx*))))
    (values (aref *dir-dx* i) (aref *dir-dy* i))))

(defun resolve-mmorpg-symbols ()
  "Resolve MMORPG function symbols after quickload to avoid package read-time errors."
  (flet ((sym (name)
           (or (find-symbol name "MMORPG")
               (error "MMORPG symbol not found: ~a" name))))
    (setf *mmorpg-make-net-buffer* (symbol-function (sym "MAKE-NET-BUFFER"))
          *mmorpg-make-intent* (symbol-function (sym "MAKE-INTENT"))
          *mmorpg-send-auth* (symbol-function (sym "SEND-AUTH-MESSAGE"))
          *mmorpg-send-net* (symbol-function (sym "SEND-NET-MESSAGE"))
          *mmorpg-receive-net* (symbol-function (sym "RECEIVE-NET-MESSAGE"))
          *mmorpg-intent->plist* (symbol-function (sym "INTENT->PLIST"))
          *mmorpg-set-dx* (fdefinition (list 'setf (sym "INTENT-MOVE-DX")))
          *mmorpg-set-dy* (fdefinition (list 'setf (sym "INTENT-MOVE-DY"))))))

(defun resolve-usocket-symbols ()
  "Resolve usocket function symbols after quickload to avoid package read-time errors."
  (flet ((sym (name)
           (or (find-symbol name "USOCKET")
               (error "USOCKET symbol not found: ~a" name))))
    (setf *usocket-connect* (symbol-function (sym "SOCKET-CONNECT"))
          *usocket-close* (symbol-function (sym "SOCKET-CLOSE")))))

(defun resolve-ql-symbols ()
  "Resolve Quicklisp function symbols after loading quicklisp."
  (flet ((sym (name)
           (or (find-symbol name "QL")
               (error "QL symbol not found: ~a" name))))
    (setf *ql-register* (symbol-function (sym "REGISTER-LOCAL-PROJECTS"))
          *ql-quickload* (symbol-function (sym "QUICKLOAD")))))

(defun send-auth-once (client host port now)
  (setf (stress-client-last-auth-time client) now)
  (funcall *mmorpg-send-auth*
           (stress-client-socket client)
           :register
           (stress-client-username client)
           "test"
           :host host :port port))

(defun spawn-client (host port now)
  "Create a client socket and send a single register request. Returns client or NIL on failure."
  (let* ((id (next-id))
         (username (format nil "stress-~d-~d" *run-id* id))
         (socket nil)
         (buffer (funcall *mmorpg-make-net-buffer*))
         (intent (funcall *mmorpg-make-intent*)))
    (handler-case
        (progn
          (setf socket (funcall *usocket-connect* host port :protocol :datagram))
          (let ((client (%make-stress-client
                         :id id
                         :username username
                         :socket socket
                         :buffer buffer
                         :intent intent
                         :authenticated-p nil
                         :last-auth-time 0.0f0
                         :next-send-time 0.0f0
                         :next-dir-change 0.0f0)))
            (send-auth-once client host port now)
            client))
      (error ()
        (when socket
          (ignore-errors (funcall *usocket-close* socket)))
        nil))))

(defun drain-auth (client now)
  "Check for auth response on a pending client. Returns :ok, :fail, or :pending."
  (multiple-value-bind (msg _host _port)
      (funcall *mmorpg-receive-net*
               (stress-client-socket client)
               (stress-client-buffer client))
    (declare (ignore _host _port))
    (cond
      ((and msg (eq (getf msg :type) :auth-ok))
       (incf *auth-ok-count*)
       :ok)
      ((and msg (eq (getf msg :type) :auth-fail))
       (incf *auth-fail-count*)
       :fail)
      ((>= (- now (stress-client-last-auth-time client)) *auth-timeout*)
       (incf *auth-timeout-count*)
       :fail)
      (t :pending))))

(defun step-client (client host port now send-interval dir-interval drain-max)
  "Advance one client state: change direction, send intent, drain snapshots."
  (when (>= now (stress-client-next-dir-change client))
    (multiple-value-bind (dx dy) (random-direction)
      (funcall *mmorpg-set-dx* dx (stress-client-intent client))
      (funcall *mmorpg-set-dy* dy (stress-client-intent client))
      (setf (stress-client-next-dir-change client) (+ now dir-interval))))
  (when (>= now (stress-client-next-send-time client))
    (funcall *mmorpg-send-net*
             (stress-client-socket client)
             (list :type :intent
                   :payload (funcall *mmorpg-intent->plist*
                                     (stress-client-intent client)))
             :host host :port port)
    (setf (stress-client-next-send-time client) (+ now send-interval)))
  ;; Drain snapshots (non-blocking)
  (dotimes (_ drain-max)
    (multiple-value-bind (msg _host _port)
        (funcall *mmorpg-receive-net*
                 (stress-client-socket client)
                 (stress-client-buffer client))
      (declare (ignore _host _port))
      (unless msg
        (return)))))

(defun main ()
  (load-quicklisp)
  (resolve-ql-symbols)
  (funcall *ql-register*)
  (funcall *ql-quickload* :mmorpg :silent t)
  (resolve-mmorpg-symbols)
  (resolve-usocket-symbols)

  (let* ((host (or (sb-ext:posix-getenv "MMORPG_HOST") "127.0.0.1"))
         (port 1337)
         (send-interval (if (> *send-hz* 0.0f0) (/ 1.0f0 *send-hz*) 0.2f0))
         (pending (make-array 0 :adjustable t :fill-pointer 0))
         (clients (make-array 0 :adjustable t :fill-pointer 0))
         (total-spawned 0)
         (active 0)
         (dropped 0)
         (start (now-seconds))
         (next-spawn start)
         (target-reached nil)
         (last-report start))

    (format t "~&=== Stress Test (Simple) ===~%")
    (format t "Host: ~a:~d~%" host port)
    (format t "Target clients: ~d~%" *target*)
    (format t "Spawn interval: ~,2f s (1 client at a time)~%" *spawn-interval*)
    (format t "Auth timeout: ~,2f s (no retries)~%" *auth-timeout*)
    (format t "Send rate: ~,2f Hz per client~%" *send-hz*)
    (format t "Tick: ~,2f s | Dir change: ~,2f s | Drain: ~d~%"
            *tick* *dir-interval* *drain-max*)
    (finish-output)

    (unwind-protect
         (loop
           :do (let ((now (now-seconds)))
                 ;; Spawn one client at a time, every 0.5s
                 (when (and (< (+ active (length pending)) *target*)
                            (>= now next-spawn))
                   (let ((client (spawn-client host port now)))
                     (when client
                       (vector-push-extend client pending)
                       (incf total-spawned))
                     (setf next-spawn (+ now *spawn-interval*))))

                 ;; Promote pending clients that authenticated
                 (let ((i 0))
                   (loop :while (< i (length pending))
                         :do (let ((client (aref pending i)))
                               (case (drain-auth client now)
                                 (:ok
                                  (setf (stress-client-authenticated-p client) t)
                                  (multiple-value-bind (dx dy) (random-direction)
                                    (funcall *mmorpg-set-dx* dx (stress-client-intent client))
                                    (funcall *mmorpg-set-dy* dy (stress-client-intent client)))
                                  (setf (stress-client-next-send-time client)
                                        (+ now (random send-interval))
                                        (stress-client-next-dir-change client)
                                        (+ now (random *dir-interval*)))
                                  (vector-push-extend client clients)
                                  (incf active)
                                  (let ((last-idx (1- (length pending))))
                                    (setf (aref pending i) (aref pending last-idx))
                                    (decf (fill-pointer pending))))
                                 (:fail
                                  (incf *auth-drop-count*)
                                  (incf dropped)
                                  (ignore-errors (funcall *usocket-close*
                                                          (stress-client-socket client)))
                                  (let ((last-idx (1- (length pending))))
                                    (setf (aref pending i) (aref pending last-idx))
                                    (decf (fill-pointer pending))))
                                 (t
                                  (incf i))))))

                 (when (and (not target-reached) (>= active *target*))
                   (setf target-reached t)
                   (format t "[STRESS] Target active clients reached: ~d~%" active)
                   (finish-output))

                 ;; Step active clients
                 (loop :for i :from 0 :below (length clients)
                       :for client = (aref clients i)
                       :do (step-client client host port now send-interval *dir-interval* *drain-max*))

                 ;; Periodic report
                 (when (>= (- now last-report) 5.0f0)
                   (setf last-report now)
                   (format t "[STRESS] Active: ~d | Pending: ~d | Spawned: ~d | Dropped: ~d~%"
                           active (length pending) total-spawned dropped)
                   (format t "         Auth ok=~d fail=~d timeout=~d drop=~d~%"
                           *auth-ok-count* *auth-fail-count* *auth-timeout-count* *auth-drop-count*)
                   (finish-output))

                 (sleep *tick*)))
      ;; Cleanup
      (loop :for client :across clients
            :do (ignore-errors (funcall *usocket-close*
                                        (stress-client-socket client))))
      (loop :for client :across pending
            :do (ignore-errors (funcall *usocket-close*
                                        (stress-client-socket client))))
      (format t "~&[STRESS] done. Spawned=~d Active=~d Pending=~d Dropped=~d~%"
              total-spawned active (length pending) dropped)
      (finish-output))))

(main)
