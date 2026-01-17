# Deployment & DevOps

Operations documentation for the MMORPG server infrastructure.

## Redis/Valkey Configuration

### Check Current Settings

```bash
redis-cli CONFIG GET appendonly
redis-cli CONFIG GET save
```

### Required Settings

Redis defaults that should already be set:
- ✅ `appendfsync everysec` - already set
- ✅ `auto-aof-rewrite-percentage 100` - already set
- ✅ `auto-aof-rewrite-min-size 64mb` - already set
- ✅ `dir /var/lib/valkey/` - data directory configured
- ✅ `stop-writes-on-bgsave-error yes` - safety enabled

Settings you may need to enable:
- ❌ `appendonly no` - Needs to be `yes` (critical for durability)
- ❌ No save directives - Needs RDB snapshots for backups

### Enable Missing Settings

```bash
sudo sed -i 's/^appendonly no$/appendonly yes/' /etc/valkey/valkey.conf
echo -e '\n# RDB snapshot settings for MMORPG persistence\nsave 300 1\nsave 60 1000' | sudo tee -a /etc/valkey/valkey.conf
sudo systemctl restart valkey
systemctl status valkey
```

### Verify Persistence is Working

The key lines in `systemctl status valkey` should show:
```
Creating AOF base file appendonly.aof.1.base.rdb on server start
Creating AOF incr file appendonly.aof.1.incr.aof on server start
Ready to accept connections tcp
```

With these settings:
- **AOF**: Logs every write operation (max 1 second data loss)
- **RDB**: Snapshots every 5 minutes (if anything changed) or every 1 minute (if 1000+ changes)

## Automated Backups

Redis backups are handled via systemd timer. Backups are stored in `/var/mmorpg/db/backups/` with timestamps and automatic rotation.

### Install Backup System

```bash
# Copy script and make executable
sudo cp deploy/mmorpg-backup.sh /usr/local/bin/
sudo chmod +x /usr/local/bin/mmorpg-backup.sh

# Create backup directory
sudo mkdir -p /var/mmorpg/db/backups

# Install systemd units
sudo cp deploy/mmorpg-backup.service /etc/systemd/system/
sudo cp deploy/mmorpg-backup.timer /etc/systemd/system/

# Enable and start the timer
sudo systemctl daemon-reload
sudo systemctl enable mmorpg-backup.timer
sudo systemctl start mmorpg-backup.timer

# Verify timer is active
systemctl list-timers | grep mmorpg
```

### Backup Configuration

Environment variables (set in `/etc/systemd/system/mmorpg-backup.service`):

| Variable | Default | Description |
|----------|---------|-------------|
| `MMORPG_BACKUP_DIR` | `/var/mmorpg/db/backups` | Where backups are stored |
| `MMORPG_REDIS_HOST` | `127.0.0.1` | Redis host |
| `MMORPG_REDIS_PORT` | `6379` | Redis port |
| `MMORPG_REDIS_DATA_DIR` | `/var/lib/valkey` | Where Redis stores dump.rdb |
| `MMORPG_KEEP_BACKUPS` | `168` | Number of backups to keep (168 = 7 days hourly) |

### Manual Backup

```bash
# Run backup immediately
sudo systemctl start mmorpg-backup.service

# Check backup status
sudo journalctl -u mmorpg-backup.service -n 20

# List backups
ls -lh /var/mmorpg/db/backups/
```

### Restore from Backup

```bash
# Stop game server first!
sudo systemctl stop mmorpg-server  # or however you run it

# Stop Redis
sudo systemctl stop valkey

# Replace dump.rdb with backup
sudo cp /var/mmorpg/db/backups/dump-YYYYMMDD-HHMMSS.rdb /var/lib/valkey/dump.rdb
sudo chown valkey:valkey /var/lib/valkey/dump.rdb

# Start Redis (will load the backup)
sudo systemctl start valkey

# Start game server
sudo systemctl start mmorpg-server
```

### Performance Note

Redis `BGSAVE` runs in a forked child process - the main Redis continues serving requests without interruption. For our game scale, backup overhead is negligible.

## Files in This Directory

| File | Purpose |
|------|---------|
| `mmorpg-backup.sh` | Backup script (triggers BGSAVE, copies dump, rotates old backups) |
| `mmorpg-backup.service` | systemd oneshot service for running backups |
| `mmorpg-backup.timer` | systemd timer (runs hourly at :05) |
