#!/bin/bash

# Deploy Go-Ichiran API to Ubuntu 22.04 LTS VPS
# This script installs required dependencies and deploys the application

set -e

echo "=== Go-Ichiran API VPS Deployment Script ==="
echo "Target: Ubuntu 22.04 LTS"
echo "Date: $(date)"
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if running as root
if [[ $EUID -eq 0 ]]; then
   log_error "This script should not be run as root for security reasons."
   log_info "Please run as a regular user with sudo privileges."
   exit 1
fi

# Update system
log_info "Updating system packages..."
sudo apt-get update
sudo apt-get upgrade -y

# Install essential packages
log_info "Installing essential packages..."
sudo apt-get install -y \
    curl \
    wget \
    git \
    unzip \
    software-properties-common \
    apt-transport-https \
    ca-certificates \
    gnupg \
    lsb-release

# Install Docker
log_info "Installing Docker..."
if ! command -v docker &> /dev/null; then
    # Add Docker's official GPG key
    sudo mkdir -p /etc/apt/keyrings
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg

    # Set up the repository
    echo \
        "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
        $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

    # Install Docker Engine
    sudo apt-get update
    sudo apt-get install -y docker-ce docker-ce-cli containerd.io docker-compose-plugin

    # Add user to docker group
    sudo usermod -aG docker $USER
    log_warn "You need to log out and back in for docker group changes to take effect."
else
    log_info "Docker is already installed."
fi

# Install Docker Compose (standalone)
log_info "Installing Docker Compose..."
if ! command -v docker-compose &> /dev/null; then
    sudo curl -L "https://github.com/docker/compose/releases/latest/download/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
    sudo chmod +x /usr/local/bin/docker-compose
else
    log_info "Docker Compose is already installed."
fi

# Install Go
log_info "Installing Go programming language..."
GO_VERSION="1.21.5"
if ! command -v go &> /dev/null; then
    cd /tmp
    wget https://go.dev/dl/go${GO_VERSION}.linux-amd64.tar.gz
    sudo rm -rf /usr/local/go
    sudo tar -C /usr/local -xzf go${GO_VERSION}.linux-amd64.tar.gz
    
    # Add Go to PATH
    echo 'export PATH=$PATH:/usr/local/go/bin' | sudo tee -a /etc/profile
    echo 'export PATH=$PATH:/usr/local/go/bin' >> ~/.bashrc
    export PATH=$PATH:/usr/local/go/bin
    
    # Verify installation
    go version
    log_info "Go ${GO_VERSION} installed successfully."
else
    log_info "Go is already installed: $(go version)"
fi

# Create application directory
APP_DIR="/opt/go-ichiran-api"
log_info "Creating application directory: $APP_DIR"
sudo mkdir -p $APP_DIR
sudo chown $USER:$USER $APP_DIR

# Clone or copy application files
log_info "Setting up application files..."
cd $APP_DIR

# If running from the project directory, copy files
if [ -f "../docker-compose.yaml" ]; then
    log_info "Copying files from current directory..."
    cp -r ../* .
else
    log_warn "Application files not found in parent directory."
    log_info "Please copy your application files to $APP_DIR"
    log_info "Required files:"
    echo "  - docker-compose.yaml"
    echo "  - Dockerfile"
    echo "  - Dockerfile.ichiran"
    echo "  - code/"
    echo "  - scripts/"
    echo "  - nginx.conf"
fi

# Create necessary directories
mkdir -p logs ssl init-db

# Set permissions
chmod +x scripts/*.sh

# Create environment file
log_info "Creating environment configuration..."
cat > .env << EOF
# Go-Ichiran API Configuration
COMPOSE_PROJECT_NAME=go-ichiran-api

# Database Configuration
POSTGRES_DB=ichiran
POSTGRES_USER=ichiran
POSTGRES_PASSWORD=$(openssl rand -base64 32)

# API Configuration
PORT=8080
GIN_MODE=release

# Nginx Configuration
HTTP_PORT=80
HTTPS_PORT=443
EOF

log_info "Environment file created at $APP_DIR/.env"

# Create systemd service for auto-start
log_info "Creating systemd service..."
sudo tee /etc/systemd/system/go-ichiran-api.service > /dev/null << EOF
[Unit]
Description=Go-Ichiran API Service
Requires=docker.service
After=docker.service

[Service]
Type=oneshot
RemainAfterExit=yes
WorkingDirectory=$APP_DIR
ExecStart=/usr/local/bin/docker-compose up -d
ExecStop=/usr/local/bin/docker-compose down
TimeoutStartSec=0
User=$USER
Group=$USER

[Install]
WantedBy=multi-user.target
EOF

# Enable and start the service
sudo systemctl daemon-reload
sudo systemctl enable go-ichiran-api.service

# Configure firewall (if ufw is available)
if command -v ufw &> /dev/null; then
    log_info "Configuring firewall..."
    sudo ufw allow 22/tcp
    sudo ufw allow 80/tcp
    sudo ufw allow 443/tcp
    # Don't enable automatically to avoid locking out users
    log_warn "Firewall rules added but not enabled. Run 'sudo ufw enable' when ready."
fi

# Create SSL directory and self-signed certificate for testing
log_info "Creating SSL directory and self-signed certificate..."
mkdir -p ssl
openssl req -x509 -nodes -days 365 -newkey rsa:2048 \
    -keyout ssl/key.pem \
    -out ssl/cert.pem \
    -subj "/C=US/ST=State/L=City/O=Organization/CN=localhost"

log_info "Self-signed SSL certificate created for testing."

# Create deployment script
log_info "Creating deployment management scripts..."

cat > deploy.sh << 'EOF'
#!/bin/bash
# Deployment management script

case "$1" in
    start)
        echo "Starting Go-Ichiran API..."
        docker-compose up -d
        ;;
    stop)
        echo "Stopping Go-Ichiran API..."
        docker-compose down
        ;;
    restart)
        echo "Restarting Go-Ichiran API..."
        docker-compose down
        docker-compose up -d
        ;;
    logs)
        docker-compose logs -f
        ;;
    status)
        docker-compose ps
        ;;
    update)
        echo "Updating and rebuilding..."
        docker-compose down
        docker-compose build --no-cache
        docker-compose up -d
        ;;
    *)
        echo "Usage: $0 {start|stop|restart|logs|status|update}"
        exit 1
        ;;
esac
EOF

chmod +x deploy.sh

# Create monitoring script
cat > monitor.sh << 'EOF'
#!/bin/bash
# Basic monitoring script

echo "=== Go-Ichiran API Status ==="
echo "Date: $(date)"
echo ""

echo "=== Docker Containers ==="
docker-compose ps

echo ""
echo "=== Container Resource Usage ==="
docker stats --no-stream $(docker-compose ps -q)

echo ""
echo "=== API Health Check ==="
curl -s http://localhost:8080/health | jq . || echo "Health check failed"

echo ""
echo "=== Nginx Access Logs (last 10 lines) ==="
docker-compose logs --tail=10 nginx

echo ""
echo "=== Disk Usage ==="
df -h

echo ""
echo "=== Memory Usage ==="
free -h
EOF

chmod +x monitor.sh

# Final instructions
log_info "Deployment script completed successfully!"
echo ""
echo "=== Next Steps ==="
echo "1. Copy your application files to $APP_DIR if not already done"
echo "2. Review and modify .env file if needed"
echo "3. Start the application:"
echo "   cd $APP_DIR"
echo "   ./deploy.sh start"
echo ""
echo "=== Management Commands ==="
echo "Start:    ./deploy.sh start"
echo "Stop:     ./deploy.sh stop"
echo "Restart:  ./deploy.sh restart"
echo "Logs:     ./deploy.sh logs"
echo "Status:   ./deploy.sh status"
echo "Update:   ./deploy.sh update"
echo "Monitor:  ./monitor.sh"
echo ""
echo "=== Service Management ==="
echo "Enable auto-start:  sudo systemctl enable go-ichiran-api"
echo "Start service:      sudo systemctl start go-ichiran-api"
echo "Stop service:       sudo systemctl stop go-ichiran-api"
echo "Service status:     sudo systemctl status go-ichiran-api"
echo ""
echo "=== API Endpoints ==="
echo "Health Check: http://your-server/health"
echo "Text Analysis: POST http://your-server/api/v1/analyze"
echo "Romanization: POST http://your-server/api/v1/romanize"
echo "Kanji Analysis: POST http://your-server/api/v1/kanji"
echo "Dictionary: POST http://your-server/api/v1/dictionary"
echo ""
log_warn "Remember to:"
log_warn "1. Configure your domain name and SSL certificates for production"
log_warn "2. Review security settings and firewall rules"
log_warn "3. Set up monitoring and backup solutions"
log_warn "4. Change default passwords in .env file"

echo ""
log_info "Installation complete! The application is ready to deploy."
