
# DigitalOcean Deployment Guide for Aralex (lisanarab.com)

## Prerequisites

✅ Docker image on Docker Hub: `alqatari/aralex:latest` (ARM64)
✅ Domain: `lisanarab.com`

## Cost: $6/month

- **Droplet:** $6/month (Basic, 1GB RAM, 25GB SSD, 1TB transfer)
- **SSL Certificate:** FREE (Let's Encrypt)
- **Total:** $6/month

## Step-by-Step Deployment

### 1. Create DigitalOcean Droplet

1. Go to: https://cloud.digitalocean.com/
2. Sign up/Login
3. Click **Create** → **Droplets**

**Droplet Configuration:**

#### Choose Region
- **Recommendation:** Frankfurt (closest to Middle East/Arab users)
- **Alternatives:** London, Amsterdam

#### Choose an Image
- **Distribution:** Ubuntu 24.04 LTS x64

#### Choose Size
- **Droplet Type:** Basic
- **CPU Options:** Regular
- **Select:** **$6/mo** - 1 GB / 1 CPU, 25 GB SSD, 1000 GB transfer

#### Choose Authentication Method
**Option A: SSH Key (Recommended)**
- Click "New SSH Key"
- Paste your public key from `~/.ssh/id_rsa.pub`
- Name it (e.g., "My Mac")

**Option B: Password**
- DigitalOcean will email you root password
- Less secure, but simpler

#### Finalize Details
- **Hostname:** `lisanarab`
- **Tags:** (optional) `production`, `aralex`
- **Backups:** OFF (saves $1.20/month) - You can enable later

4. Click **Create Droplet**
5. **Note the IP address** (e.g., `147.182.x.x`)

### 2. Configure DNS (Do this BEFORE running the script)

**If using DigitalOcean DNS:**
1. Go to **Networking** → **Domains**
2. Add domain: `lisanarab.com`
3. Create DNS records:
```
Type    Hostname    Value               TTL
A       @           YOUR_DROPLET_IP     3600
A       www         YOUR_DROPLET_IP     3600
```

4. Update your domain registrar's nameservers to:
```
ns1.digitalocean.com
ns2.digitalocean.com
ns3.digitalocean.com
```

**If using external registrar (Namecheap, GoDaddy, etc.):**
- Add A records directly at your registrar:
```
A       @           YOUR_DROPLET_IP
A       www         YOUR_DROPLET_IP
```

**Wait 5-10 minutes** for DNS propagation

### 3. Connect to Your Droplet

**Using SSH Key:**
```bash
ssh root@YOUR_DROPLET_IP
```

**Using Password:**
```bash
ssh root@YOUR_DROPLET_IP
# Enter password from email
# You'll be prompted to change password on first login
```

### 4. Upload and Run Deployment Script

**Option A: SCP (from your Mac)**

From your local machine:
```bash
cd /Users/alqatari/agent/lisan/aralex
scp deploy-digitalocean.sh root@YOUR_DROPLET_IP:/root/
```

Then on the Droplet:
```bash
chmod +x /root/deploy-digitalocean.sh
sudo /root/deploy-digitalocean.sh
```

**Option B: Copy-Paste**

On the Droplet:
```bash
nano deploy.sh
```

Paste the entire script content, save (Ctrl+X, Y, Enter), then:
```bash
chmod +x deploy.sh
sudo ./deploy.sh
```

**Option C: Direct wget (if script is on GitHub)**
```bash
wget https://raw.githubusercontent.com/YOUR_REPO/deploy-digitalocean.sh
chmod +x deploy-digitalocean.sh
sudo ./deploy-digitalocean.sh
```

### 5. Monitor Deployment

The script will automatically:
- ✅ Update system packages
- ✅ Install Docker
- ✅ Pull `alqatari/aralex:latest` from Docker Hub
- ✅ Run the container
- ✅ Install and configure Nginx
- ✅ Get SSL certificate from Let's Encrypt
- ✅ Configure UFW firewall

**Total time:** ~5-10 minutes

### 6. Verify Deployment

**Check container status:**
```bash
sudo docker ps
# Should show: aralex container running
```

**Check container logs:**
```bash
sudo docker logs -f aralex
# Press Ctrl+C to exit
```

**Check Nginx:**
```bash
sudo systemctl status nginx
```

**Test the site:**
```bash
curl https://lisanarab.com
```

**From browser:**
- https://lisanarab.com
- https://www.lisanarab.com

## Troubleshooting

### Container won't start
```bash
# Check logs
sudo docker logs aralex

# Check if port is already in use
sudo lsof -i :8080

# Restart container
sudo docker restart aralex
```

### Nginx errors
```bash
# Test configuration
sudo nginx -t

# Check status
sudo systemctl status nginx

# View error logs
sudo tail -f /var/log/nginx/error.log
```

### SSL certificate failed

**Error:** `Challenge failed` or `DNS resolution failed`

**Solution:**
1. Verify DNS is correct:
```bash
dig lisanarab.com +short
# Should return your Droplet IP
```

2. Wait 10-15 minutes for DNS propagation

3. Retry SSL:
```bash
sudo certbot --nginx -d lisanarab.com -d www.lisanarab.com
```

### Firewall blocking connections
```bash
# Check firewall status
sudo ufw status

# Allow HTTP/HTTPS
sudo ufw allow 80/tcp
sudo ufw allow 443/tcp
sudo ufw reload
```

### Can't SSH to server
```bash
# From DigitalOcean control panel:
# Click Droplet → Access → Launch Console
# This gives you browser-based terminal access
```

## Maintenance Commands

### Update Application
```bash
# Pull latest image
sudo docker pull alqatari/aralex:latest

# Stop and remove old container
sudo docker stop aralex
sudo docker rm aralex

# Run new container
sudo docker run -d \
  --name aralex \
  --restart unless-stopped \
  -p 127.0.0.1:8080:8080 \
  alqatari/aralex:latest
```

### View Logs
```bash
# Application logs
sudo docker logs -f aralex

# Nginx access logs
sudo tail -f /var/log/nginx/access.log

# Nginx error logs
sudo tail -f /var/log/nginx/error.log

# System logs
sudo journalctl -xe
```

### Restart Services
```bash
# Restart container
sudo docker restart aralex

# Restart Nginx
sudo systemctl restart nginx

# Restart both
sudo docker restart aralex && sudo systemctl restart nginx
```

### SSL Certificate Renewal
```bash
# Test renewal (dry run)
sudo certbot renew --dry-run

# Force renewal
sudo certbot renew --force-renewal

# Auto-renewal is already configured via systemd timer
sudo systemctl status certbot.timer
```

### Backup Databases
```bash
# Create backup
sudo docker exec aralex tar -czf /tmp/backup.tar.gz /app/backend/*.db

# Copy to host
sudo docker cp aralex:/tmp/backup.tar.gz ~/aralex-backup-$(date +%Y%m%d).tar.gz

# Download to your Mac (from local machine)
scp root@YOUR_DROPLET_IP:~/aralex-backup-*.tar.gz ~/Downloads/
```

### Monitor Resources
```bash
# Check disk usage
df -h

# Check memory usage
free -h

# Check CPU and processes
htop
# (Install with: sudo apt-get install htop)
```

## Security Best Practices

### 1. Disable Root SSH Login (After setting up sudo user)
```bash
# Create non-root user
adduser deploy
usermod -aG sudo deploy
usermod -aG docker deploy

# Test login as new user
exit
ssh deploy@YOUR_DROPLET_IP

# Disable root login
sudo nano /etc/ssh/sshd_config
# Set: PermitRootLogin no
sudo systemctl restart sshd
```

### 2. Enable Automatic Security Updates
```bash
sudo apt-get install unattended-upgrades
sudo dpkg-reconfigure -plow unattended-upgrades
```

### 3. Setup Fail2Ban (Prevents brute force attacks)
```bash
sudo apt-get install fail2ban
sudo systemctl enable fail2ban
sudo systemctl start fail2ban
```

### 4. Monitor Failed Login Attempts
```bash
sudo tail -f /var/log/auth.log
```

## DigitalOcean Features

### Monitoring
- **Droplet Graphs:** CPU, Memory, Disk I/O, Bandwidth
- **Alerts:** Set up email alerts for high resource usage
- **Go to:** Droplet → Monitoring

### Backups
- **Enable Backups:** 20% of monthly cost ($1.20/mo extra)
- **Weekly automated backups**
- **Restore from backup in seconds**
- **Go to:** Droplet → Backups

### Snapshots
- **Manual snapshots:** $0.05/GB/month ($1.25/month for 25GB)
- **Good for:** Before major updates
- **Go to:** Droplet → Snapshots

### Floating IPs
- **Static IP that doesn't change**
- **Free** while assigned to a Droplet
- **Useful for:** Switching between Droplets without DNS changes

### Resize Droplet
- **Upgrade RAM/CPU:** Without losing data
- **Downgrade:** Only works if you resize disk too
- **Go to:** Droplet → Resize

## Cost Breakdown

| Item | Cost |
|------|------|
| Basic Droplet (1GB RAM) | $6.00/month |
| SSL Certificate (Let's Encrypt) | FREE |
| Bandwidth (1TB included) | FREE |
| Backups (optional) | +$1.20/month |
| **Total (without backups)** | **$6.00/month** |
| **Total (with backups)** | **$7.20/month** |

## Comparison: DigitalOcean vs Hetzner

| Feature | DigitalOcean | Hetzner |
|---------|--------------|---------|
| Price | $6/month | €4.51/month (~$5) |
| RAM | 1 GB | 4 GB |
| Storage | 25 GB SSD | 40 GB SSD |
| Transfer | 1 TB | 20 TB |
| Support | Email/Tickets | Email/Tickets |
| UI/UX | ⭐⭐⭐⭐⭐ Excellent | ⭐⭐⭐⭐ Good |
| Docs | ⭐⭐⭐⭐⭐ Excellent | ⭐⭐⭐⭐ Good |
| US Regions | ✅ Yes | ❌ No |
| EU Regions | ✅ Yes | ✅ Yes |

**Verdict:**
- **DigitalOcean:** Better UX, easier for beginners, more expensive
- **Hetzner:** Better value, more RAM, EU-only locations

## Support Resources

- **DigitalOcean Docs:** https://docs.digitalocean.com/
- **Community Tutorials:** https://www.digitalocean.com/community/tutorials
- **Support Tickets:** https://cloud.digitalocean.com/support/tickets
- **Status Page:** https://status.digitalocean.com/

---

**Last Updated:** October 2025
**Deployment Script:** `deploy-digitalocean.sh`
**Architecture:** ARM64 (Apple Silicon compatible)
