# Build stage
FROM golang:1.21-alpine AS builder

WORKDIR /app

# Install git and other dependencies
RUN apk add --no-cache git

# Copy go mod files
COPY code/go.mod code/go.sum ./

# Download dependencies
RUN go mod download

# Copy source code
COPY code/ .

# Build the application
RUN CGO_ENABLED=0 GOOS=linux go build -a -installsuffix cgo -o main .

# Final stage
FROM alpine:latest

# Install ca-certificates and docker client
RUN apk --no-cache add ca-certificates docker-cli

WORKDIR /root/

# Copy the binary from builder stage
COPY --from=builder /app/main .

# Expose port
EXPOSE 8080

# Command to run
CMD ["./main"]
