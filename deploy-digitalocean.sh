#!/bin/bash
set -e

# ============================================================
# Aralex Deployment Script for DigitalOcean
# Domain: lisanarab.com
# ============================================================

echo "🚀 Starting Aralex deployment to DigitalOcean..."
echo ""

# Configuration
DOMAIN="lisanarab.com"
DOCKER_IMAGE="alqatari/aralex:amd64"
CONTAINER_NAME="aralex"
EMAIL="alqatari@me.com"  # For Let's Encrypt SSL

# ============================================================
# Step 1: System Update
# ============================================================
echo "📦 Step 1/7: Updating system packages..."
sudo apt-get update -qq
sudo apt-get upgrade -y -qq
echo "✅ System updated"
echo ""

# ============================================================
# Step 2: Install Docker
# ============================================================
echo "🐋 Step 2/7: Installing Docker..."

# Remove old versions if any
sudo apt-get remove -y docker docker-engine docker.io containerd runc 2>/dev/null || true

# Install dependencies
sudo apt-get install -y -qq \
    ca-certificates \
    curl \
    gnupg \
    lsb-release

# Add Docker's official GPG key
sudo mkdir -p /etc/apt/keyrings
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg

# Set up Docker repository
echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

# Install Docker Engine
sudo apt-get update -qq
sudo apt-get install -y -qq docker-ce docker-ce-cli containerd.io docker-compose-plugin

# Start and enable Docker
sudo systemctl start docker
sudo systemctl enable docker

# Add current user to docker group
sudo usermod -aG docker $USER

echo "✅ Docker installed successfully"
echo ""

# ============================================================
# Step 3: Pull and Run Aralex Container
# ============================================================
echo "📥 Step 3/7: Pulling Aralex Docker image..."
sudo docker pull $DOCKER_IMAGE

echo "🚀 Starting Aralex container..."

# Stop and remove existing container if it exists
sudo docker stop $CONTAINER_NAME 2>/dev/null || true
sudo docker rm $CONTAINER_NAME 2>/dev/null || true

# Run the container
sudo docker run -d \
  --name $CONTAINER_NAME \
  --restart unless-stopped \
  -p 127.0.0.1:8080:8080 \
  $DOCKER_IMAGE

# Wait for container to be healthy
echo "⏳ Waiting for container to start..."
sleep 5

# Check if container is running
if sudo docker ps | grep -q $CONTAINER_NAME; then
    echo "✅ Aralex container is running"
else
    echo "❌ ERROR: Container failed to start"
    sudo docker logs $CONTAINER_NAME
    exit 1
fi
echo ""

# ============================================================
# Step 4: Install Nginx
# ============================================================
echo "🌐 Step 4/7: Installing Nginx..."
sudo apt-get install -y -qq nginx

# Start and enable Nginx
sudo systemctl start nginx
sudo systemctl enable nginx

echo "✅ Nginx installed"
echo ""

# ============================================================
# Step 5: Configure Nginx Reverse Proxy
# ============================================================
echo "⚙️  Step 5/7: Configuring Nginx for $DOMAIN..."

# Create Nginx configuration
sudo tee /etc/nginx/sites-available/lisanarab > /dev/null <<'EOF'
server {
    listen 80;
    listen [::]:80;
    server_name lisanarab.com www.lisanarab.com;

    # Allow large uploads if needed
    client_max_body_size 10M;

    location / {
        proxy_pass http://127.0.0.1:8080;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_cache_bypass $http_upgrade;
    }
}
EOF

# Enable the site
sudo ln -sf /etc/nginx/sites-available/lisanarab /etc/nginx/sites-enabled/

# Remove default site
sudo rm -f /etc/nginx/sites-enabled/default

# Test Nginx configuration
sudo nginx -t

# Reload Nginx
sudo systemctl reload nginx

echo "✅ Nginx configured for $DOMAIN"
echo ""

# ============================================================
# Step 6: Install Certbot and Get SSL Certificate
# ============================================================
echo "🔒 Step 6/7: Setting up SSL with Let's Encrypt..."

# Install Certbot
sudo apt-get install -y -qq certbot python3-certbot-nginx

# Get SSL certificate
echo "📝 Obtaining SSL certificate for $DOMAIN..."
echo "   (This requires DNS to be pointing to this server)"
echo ""

sudo certbot --nginx \
  -d $DOMAIN \
  -d www.$DOMAIN \
  --non-interactive \
  --agree-tos \
  --email $EMAIL \
  --redirect

# Set up auto-renewal
sudo systemctl enable certbot.timer

echo "✅ SSL certificate installed"
echo ""

# ============================================================
# Step 7: Configure Firewall
# ============================================================
echo "🔥 Step 7/7: Configuring firewall..."

# Install UFW if not present
sudo apt-get install -y -qq ufw

# Configure firewall
sudo ufw --force enable
sudo ufw default deny incoming
sudo ufw default allow outgoing
sudo ufw allow ssh
sudo ufw allow 'Nginx Full'
sudo ufw allow 80/tcp
sudo ufw allow 443/tcp

echo "✅ Firewall configured"
echo ""

# ============================================================
# Deployment Complete
# ============================================================
echo "════════════════════════════════════════════════════════"
echo "✅ Deployment Complete!"
echo "════════════════════════════════════════════════════════"
echo ""
echo "📊 Server Information:"
echo "   • Container: $CONTAINER_NAME (running on port 8080)"
echo "   • Domain: https://$DOMAIN"
echo "   • SSL: Enabled (auto-renewal configured)"
echo ""
echo "🔍 Useful Commands:"
echo "   • Check container logs:  sudo docker logs -f $CONTAINER_NAME"
echo "   • Restart container:     sudo docker restart $CONTAINER_NAME"
echo "   • Check Nginx status:    sudo systemctl status nginx"
echo "   • Check SSL renewal:     sudo certbot renew --dry-run"
echo ""
echo "⚠️  IMPORTANT: Make sure DNS records are configured:"
echo "   A    @              → $(curl -s ifconfig.me)"
echo "   A    www            → $(curl -s ifconfig.me)"
echo ""
echo "🌐 Your site should be live at: https://$DOMAIN"
echo "════════════════════════════════════════════════════════"
