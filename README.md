# Go-Ichiran API Server

A RESTful API server that provides Japanese dictionary and kanji analysis features using the [ichiran](https://github.com/tshatrov/ichiran) library. The server is built in Go and uses Docker for easy deployment.

## Features

- **Text Analysis**: Complete Japanese text segmentation and analysis
- **Romanization**: Convert Japanese text to romaji
- **Kanji Analysis**: Detailed kanji character information
- **Dictionary Lookup**: Word definitions and readings
- **RESTful API**: Clean JSON-based API endpoints
- **Dockerized Deployment**: Easy setup with Docker Compose
- **Production Ready**: Includes Nginx reverse proxy and SSL support

## Architecture

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│                 │    │                 │    │                 │
│     Nginx       │◄──►│   Go API Server │◄──►│  Ichiran (Lisp) │
│  (Reverse Proxy)│    │                 │    │                 │
│                 │    │                 │    │                 │
└─────────────────┘    └─────────────────┘    └─────────────────┘
                                                         │
                                                         ▼
                                              ┌─────────────────┐
                                              │                 │
                                              │   PostgreSQL    │
                                              │   (Dictionary   │
                                              │     Database)   │
                                              │                 │
                                              └─────────────────┘
```

## API Endpoints

### Health Check
```bash
GET /health
```

### Text Analysis
```bash
POST /api/v1/analyze
Content-Type: application/json

{
    "text": "一覧は最高だぞ"
}
```

### Romanization
```bash
POST /api/v1/romanize
Content-Type: application/json

{
    "text": "こんにちは"
}
```

### Kanji Analysis
```bash
POST /api/v1/kanji
Content-Type: application/json

{
    "kanji": "漢"
}
```

### Dictionary Lookup
```bash
POST /api/v1/dictionary
Content-Type: application/json

{
    "text": "学校"
}
```

## Response Format

All API endpoints return JSON responses in the following format:

```json
{
    "status": "success",
    "text": "input text",
    "result": {
        // Analysis results
    },
    "timestamp": "2025-06-22T18:52:49Z"
}
```

Error responses:
```json
{
    "status": "error",
    "error": "error_code",
    "message": "Detailed error message"
}
```

## Quick Start

### Prerequisites

- Ubuntu 22.04 LTS VPS
- Internet connection for downloading dependencies

### Deployment

1. **Copy files to your VPS:**
   ```bash
   scp -r . user@your-vps:/home/user/go-ichiran-api/
   ```

2. **Run the deployment script:**
   ```bash
   ssh user@your-vps
   cd go-ichiran-api
   chmod +x deploy-to-vps.sh
   ./deploy-to-vps.sh
   ```

3. **Start the application:**
   ```bash
   cd /opt/go-ichiran-api
   ./deploy.sh start
   ```

4. **Test the API:**
   ```bash
   curl -X POST http://your-server/api/v1/analyze \
        -H "Content-Type: application/json" \
        -d '{"text": "こんにちは"}'
   ```

## Manual Installation

If you prefer manual installation:

### 1. Install Dependencies

```bash
# Update system
sudo apt-get update && sudo apt-get upgrade -y

# Install Docker
curl -fsSL https://get.docker.com -o get-docker.sh
sh get-docker.sh
sudo usermod -aG docker $USER

# Install Docker Compose
sudo curl -L "https://github.com/docker/compose/releases/latest/download/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose

# Install Go (optional, for development)
wget https://go.dev/dl/go1.21.5.linux-amd64.tar.gz
sudo tar -C /usr/local -xzf go1.21.5.linux-amd64.tar.gz
echo 'export PATH=$PATH:/usr/local/go/bin' >> ~/.bashrc
```

### 2. Deploy Application

```bash
# Clone or copy the application
git clone <your-repo> go-ichiran-api
cd go-ichiran-api

# Start services
docker-compose up -d

# Check status
docker-compose ps
```

## Development

### Local Development Setup

1. **Install Go 1.21+**
2. **Install Docker and Docker Compose**
3. **Clone the repository:**
   ```bash
   git clone <repo-url>
   cd go-ichiran-api
   ```

4. **Start dependencies:**
   ```bash
   docker-compose up -d postgres ichiran
   ```

5. **Run the Go API server locally:**
   ```bash
   cd code
   go mod tidy
   go run main.go
   ```

### Building and Testing

```bash
# Build the Go binary
cd code
go build -o ../main .

# Run tests
go test ./...

# Build Docker image
docker build -t go-ichiran-api .
```

## Configuration

### Environment Variables

The application can be configured using environment variables:

- `PORT`: API server port (default: 8080)
- `GIN_MODE`: Gin framework mode (development/release)
- `DB_HOST`: PostgreSQL host
- `DB_PORT`: PostgreSQL port
- `DB_NAME`: Database name
- `DB_USER`: Database user
- `DB_PASSWORD`: Database password

### Docker Compose Configuration

Key services in `docker-compose.yaml`:

- **postgres**: PostgreSQL database for ichiran
- **ichiran**: Ichiran Common Lisp application
- **api**: Go API server
- **nginx**: Reverse proxy and load balancer

## Production Deployment

### SSL/TLS Configuration

1. **Obtain SSL certificates:**
   ```bash
   # Using Let's Encrypt (recommended)
   sudo apt install certbot
   sudo certbot certonly --standalone -d your-domain.com
   ```

2. **Update nginx.conf:**
   - Uncomment HTTPS server block
   - Update certificate paths
   - Configure your domain name

3. **Restart services:**
   ```bash
   ./deploy.sh restart
   ```

### Security Considerations

1. **Change default passwords** in `.env` file
2. **Configure firewall rules:**
   ```bash
   sudo ufw enable
   sudo ufw allow 22,80,443/tcp
   ```
3. **Regular updates:**
   ```bash
   ./deploy.sh update
   ```
4. **Monitor logs:**
   ```bash
   ./deploy.sh logs
   ./monitor.sh
   ```

### Performance Tuning

1. **Adjust container resources** in docker-compose.yaml
2. **Configure Nginx caching** for static content
3. **Scale horizontally** by adding more API containers
4. **Database optimization** for large dictionaries

## Monitoring

### Health Checks

The application includes built-in health checks:

```bash
# API health
curl http://localhost:8080/health

# Container health
docker-compose ps

# Resource usage
./monitor.sh
```

### Logging

Logs are available through Docker Compose:

```bash
# All services
docker-compose logs -f

# Specific service
docker-compose logs -f api
docker-compose logs -f ichiran
docker-compose logs -f nginx
```

## Troubleshooting

### Common Issues

1. **Ichiran container fails to start:**
   - Check PostgreSQL connectivity
   - Verify dictionary data download
   - Review ichiran logs: `docker-compose logs ichiran`

2. **API returns errors:**
   - Verify ichiran container health
   - Check API logs: `docker-compose logs api`
   - Test ichiran CLI directly: `docker exec ichiran-container ichiran-cli --help`

3. **Performance issues:**
   - Monitor resource usage: `./monitor.sh`
   - Scale containers: `docker-compose up -d --scale api=3`
   - Optimize database queries

### Debug Commands

```bash
# Connect to containers
docker exec -it ichiran-container bash
docker exec -it go-ichiran-api bash

# Check container logs
docker-compose logs --tail=100 ichiran

# Test ichiran directly
docker exec ichiran-container ichiran-cli -i "テスト"

# Monitor resource usage
docker stats
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Submit a pull request

## License

This project is licensed under the MIT License. See LICENSE file for details.

## Acknowledgments

- [Ichiran](https://github.com/tshatrov/ichiran) - Japanese text analysis library
- [JMdict](http://www.edrdg.org/jmdict/j_jmdict.html) - Japanese dictionary
- [Kanjidic2](http://www.edrdg.org/wiki/index.php/KANJIDIC_Project) - Kanji dictionary

## Support

For issues and questions:

1. Check the troubleshooting section
2. Review existing issues on GitHub
3. Create a new issue with detailed information
4. Include logs and error messages

---

**Author**: MiniMax Agent  
**Version**: 1.0.0  
**Last Updated**: 2025-06-22
