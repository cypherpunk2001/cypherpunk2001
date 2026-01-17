#!/bin/bash
# MMORPG Redis Backup Script
# Triggers Redis BGSAVE, copies dump to timestamped backup, rotates old backups.
#
# Usage: mmorpg-backup.sh
# Typically run via systemd timer (mmorpg-backup.timer)

set -euo pipefail

BACKUP_DIR="${MMORPG_BACKUP_DIR:-/var/mmorpg/db/backups}"
REDIS_HOST="${MMORPG_REDIS_HOST:-127.0.0.1}"
REDIS_PORT="${MMORPG_REDIS_PORT:-6379}"
REDIS_DATA_DIR="${MMORPG_REDIS_DATA_DIR:-/var/lib/valkey}"
KEEP_BACKUPS="${MMORPG_KEEP_BACKUPS:-168}"  # 168 = 7 days of hourly backups

# Create backup directory if needed
mkdir -p "$BACKUP_DIR"

echo "[$(date -Iseconds)] Starting Redis backup..."

# Get last save timestamp before triggering new one
LAST_SAVE=$(redis-cli -h "$REDIS_HOST" -p "$REDIS_PORT" LASTSAVE)

# Trigger background save
redis-cli -h "$REDIS_HOST" -p "$REDIS_PORT" BGSAVE

# Wait for BGSAVE to complete (poll until LASTSAVE changes)
echo "[$(date -Iseconds)] Waiting for BGSAVE to complete..."
MAX_WAIT=300  # 5 minutes max
WAITED=0
while [ "$WAITED" -lt "$MAX_WAIT" ]; do
    sleep 2
    WAITED=$((WAITED + 2))
    CURRENT_SAVE=$(redis-cli -h "$REDIS_HOST" -p "$REDIS_PORT" LASTSAVE)
    if [ "$CURRENT_SAVE" != "$LAST_SAVE" ]; then
        echo "[$(date -Iseconds)] BGSAVE completed."
        break
    fi
done

if [ "$WAITED" -ge "$MAX_WAIT" ]; then
    echo "[$(date -Iseconds)] ERROR: BGSAVE did not complete within ${MAX_WAIT}s"
    exit 1
fi

# Copy dump.rdb to timestamped backup
TIMESTAMP=$(date +%Y%m%d-%H%M%S)
BACKUP_FILE="$BACKUP_DIR/dump-$TIMESTAMP.rdb"

cp "$REDIS_DATA_DIR/dump.rdb" "$BACKUP_FILE"
echo "[$(date -Iseconds)] Backup saved: $BACKUP_FILE"

# Rotate old backups (keep last N)
BACKUP_COUNT=$(ls -1 "$BACKUP_DIR"/dump-*.rdb 2>/dev/null | wc -l)
if [ "$BACKUP_COUNT" -gt "$KEEP_BACKUPS" ]; then
    DELETE_COUNT=$((BACKUP_COUNT - KEEP_BACKUPS))
    echo "[$(date -Iseconds)] Rotating: removing $DELETE_COUNT old backup(s)..."
    ls -1t "$BACKUP_DIR"/dump-*.rdb | tail -n "$DELETE_COUNT" | xargs rm -f
fi

echo "[$(date -Iseconds)] Backup complete. Total backups: $(ls -1 "$BACKUP_DIR"/dump-*.rdb | wc -l)"
