#!/bin/bash
set -e

# ============================================================
# Aralex Deployment Script for DigitalOcean
# Domain: lisanarab.com
# Optimized for fast redeployments (checks existing installations)
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
# Step 2: Check/Install Docker
# ============================================================
echo "🐋 Step 2/7: Checking Docker installation..."

if command -v docker &> /dev/null; then
    DOCKER_VERSION=$(docker --version | awk '{print $3}' | sed 's/,//')
    echo "✅ Docker already installed (version $DOCKER_VERSION)"
    echo "   Skipping Docker installation..."
else
    echo "   Docker not found, installing..."

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
fi
echo ""

# ============================================================
# Step 3: Pull and Run Aralex Container (ALWAYS UPDATE)
# ============================================================
echo "📥 Step 3/7: Updating Aralex Docker image..."
sudo docker pull $DOCKER_IMAGE

echo "🔄 Restarting Aralex container..."

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
# Step 4: Check/Install Nginx
# ============================================================
echo "🌐 Step 4/7: Checking Nginx installation..."

if command -v nginx &> /dev/null; then
    NGINX_VERSION=$(nginx -v 2>&1 | awk -F'/' '{print $2}')
    echo "✅ Nginx already installed (version $NGINX_VERSION)"
    echo "   Skipping Nginx installation..."
else
    echo "   Nginx not found, installing..."
    sudo apt-get install -y -qq nginx

    # Start and enable Nginx
    sudo systemctl start nginx
    sudo systemctl enable nginx

    echo "✅ Nginx installed"
fi
echo ""

# ============================================================
# Step 5: Configure Nginx Reverse Proxy
# ============================================================
echo "⚙️  Step 5/7: Configuring Nginx for $DOMAIN..."

# Check if configuration already exists and is identical
NGINX_CONFIG="/etc/nginx/sites-available/lisanarab"
CONFIG_CHANGED=false

if [ -f "$NGINX_CONFIG" ]; then
    echo "   Nginx configuration exists, checking if update needed..."
    # Create temp config to compare
    cat > /tmp/lisanarab.new <<'EOF'
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

    # Compare (ignoring SSL blocks that certbot adds)
    if ! grep -q "proxy_pass http://127.0.0.1:8080" "$NGINX_CONFIG"; then
        CONFIG_CHANGED=true
    fi

    rm /tmp/lisanarab.new
else
    CONFIG_CHANGED=true
fi

if [ "$CONFIG_CHANGED" = true ]; then
    echo "   Updating Nginx configuration..."

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

    echo "✅ Nginx configuration updated"
else
    echo "✅ Nginx configuration unchanged, skipping..."
fi
echo ""

# ============================================================
# Step 6: Check/Install SSL Certificate
# ============================================================
echo "🔒 Step 6/7: Checking SSL certificate..."

if command -v certbot &> /dev/null; then
    echo "✅ Certbot already installed"

    # Check if certificate exists for domain
    if sudo certbot certificates 2>/dev/null | grep -q "$DOMAIN"; then
        echo "✅ SSL certificate already exists for $DOMAIN"
        echo "   (Auto-renewal is handled by certbot.timer)"

        # Ensure timer is enabled
        sudo systemctl enable certbot.timer 2>/dev/null || true
    else
        echo "   Certificate not found, obtaining new certificate..."

        sudo certbot --nginx \
          -d $DOMAIN \
          -d www.$DOMAIN \
          --non-interactive \
          --agree-tos \
          --email $EMAIL \
          --redirect

        sudo systemctl enable certbot.timer
        echo "✅ SSL certificate installed"
    fi
else
    echo "   Certbot not found, installing..."

    sudo apt-get install -y -qq certbot python3-certbot-nginx

    echo "📝 Obtaining SSL certificate for $DOMAIN..."
    echo "   (This requires DNS to be pointing to this server)"

    sudo certbot --nginx \
      -d $DOMAIN \
      -d www.$DOMAIN \
      --non-interactive \
      --agree-tos \
      --email $EMAIL \
      --redirect

    sudo systemctl enable certbot.timer
    echo "✅ SSL certificate installed"
fi
echo ""

# ============================================================
# Step 7: Check/Configure Firewall
# ============================================================
echo "🔥 Step 7/7: Checking firewall configuration..."

if command -v ufw &> /dev/null; then
    UFW_STATUS=$(sudo ufw status | head -1)
    echo "✅ UFW already installed ($UFW_STATUS)"

    # Ensure critical rules are present
    sudo ufw allow ssh 2>/dev/null || true
    sudo ufw allow 'Nginx Full' 2>/dev/null || true
    sudo ufw allow 80/tcp 2>/dev/null || true
    sudo ufw allow 443/tcp 2>/dev/null || true

    # Enable if not active
    if ! sudo ufw status | grep -q "Status: active"; then
        sudo ufw --force enable
        echo "   Enabled firewall"
    fi
else
    echo "   UFW not found, installing and configuring..."

    sudo apt-get install -y -qq ufw

    sudo ufw --force enable
    sudo ufw default deny incoming
    sudo ufw default allow outgoing
    sudo ufw allow ssh
    sudo ufw allow 'Nginx Full'
    sudo ufw allow 80/tcp
    sudo ufw allow 443/tcp

    echo "✅ Firewall installed and configured"
fi
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
