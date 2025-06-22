package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"
	"os/exec"
	"strings"
	"time"

	"github.com/gin-gonic/gin"
	"github.com/gin-contrib/cors"
)

// Response structures
type AnalysisResponse struct {
	Status    string      `json:"status"`
	Text      string      `json:"text"`
	Result    interface{} `json:"result"`
	Timestamp string      `json:"timestamp"`
}

type ErrorResponse struct {
	Status  string `json:"status"`
	Error   string `json:"error"`
	Message string `json:"message"`
}

type HealthResponse struct {
	Status    string `json:"status"`
	Service   string `json:"service"`
	Version   string `json:"version"`
	Timestamp string `json:"timestamp"`
}

// Request structures
type AnalysisRequest struct {
	Text string `json:"text" binding:"required"`
}

type KanjiRequest struct {
	Kanji string `json:"kanji" binding:"required"`
}

// IchiranClient handles communication with ichiran
type IchiranClient struct {
	containerName string
}

func NewIchiranClient() *IchiranClient {
	return &IchiranClient{
		containerName: "ichiran-container",
	}
}

// ExecuteIchiranCommand runs ichiran command in Docker container
func (c *IchiranClient) ExecuteIchiranCommand(args ...string) (string, error) {
	// Prepare docker exec command
	dockerArgs := []string{"exec", c.containerName, "ichiran-cli"}
	dockerArgs = append(dockerArgs, args...)
	
	cmd := exec.Command("docker", dockerArgs...)
	output, err := cmd.CombinedOutput()
	
	if err != nil {
		return "", fmt.Errorf("ichiran command failed: %v, output: %s", err, string(output))
	}
	
	return string(output), nil
}

// AnalyzeText performs text analysis using ichiran
func (c *IchiranClient) AnalyzeText(text string) (interface{}, error) {
	output, err := c.ExecuteIchiranCommand("-f", "json", "-i", text)
	if err != nil {
		return nil, err
	}
	
	// Try to parse as JSON, if it fails return raw output
	var result interface{}
	if err := json.Unmarshal([]byte(output), &result); err != nil {
		// If JSON parsing fails, return structured output
		return map[string]interface{}{
			"raw_output": strings.TrimSpace(output),
			"text":       text,
		}, nil
	}
	
	return result, nil
}

// RomanizeText converts Japanese text to romaji
func (c *IchiranClient) RomanizeText(text string) (interface{}, error) {
	output, err := c.ExecuteIchiranCommand("-r", "-i", text)
	if err != nil {
		return nil, err
	}
	
	return map[string]interface{}{
		"original": text,
		"romaji":   strings.TrimSpace(output),
	}, nil
}

// AnalyzeKanji provides kanji analysis
func (c *IchiranClient) AnalyzeKanji(kanji string) (interface{}, error) {
	output, err := c.ExecuteIchiranCommand("-k", "-i", kanji)
	if err != nil {
		return nil, err
	}
	
	return map[string]interface{}{
		"kanji":    kanji,
		"analysis": strings.TrimSpace(output),
	}, nil
}

// Middleware for logging
func LoggerMiddleware() gin.HandlerFunc {
	return gin.LoggerWithFormatter(func(param gin.LogFormatterParams) string {
		return fmt.Sprintf("%s - [%s] \"%s %s %s %d %s \"%s\" %s\"\n",
			param.ClientIP,
			param.TimeStamp.Format(time.RFC1123),
			param.Method,
			param.Path,
			param.Request.Proto,
			param.StatusCode,
			param.Latency,
			param.Request.UserAgent(),
			param.ErrorMessage,
		)
	})
}

func main() {
	// Initialize ichiran client
	client := NewIchiranClient()
	
	// Initialize Gin router
	r := gin.New()
	r.Use(LoggerMiddleware())
	r.Use(gin.Recovery())
	
	// Configure CORS
	config := cors.DefaultConfig()
	config.AllowAllOrigins = true
	config.AllowMethods = []string{"GET", "POST", "PUT", "DELETE", "OPTIONS"}
	config.AllowHeaders = []string{"*"}
	r.Use(cors.New(config))
	
	// Health check endpoint
	r.GET("/health", func(c *gin.Context) {
		c.JSON(http.StatusOK, HealthResponse{
			Status:    "healthy",
			Service:   "go-ichiran-api",
			Version:   "1.0.0",
			Timestamp: time.Now().UTC().Format(time.RFC3339),
		})
	})
	
	// API routes
	api := r.Group("/api/v1")
	{
		// Text analysis endpoint
		api.POST("/analyze", func(c *gin.Context) {
			var req AnalysisRequest
			if err := c.ShouldBindJSON(&req); err != nil {
				c.JSON(http.StatusBadRequest, ErrorResponse{
					Status:  "error",
					Error:   "invalid_request",
					Message: err.Error(),
				})
				return
			}
			
			result, err := client.AnalyzeText(req.Text)
			if err != nil {
				log.Printf("Analysis error: %v", err)
				c.JSON(http.StatusInternalServerError, ErrorResponse{
					Status:  "error",
					Error:   "analysis_failed",
					Message: err.Error(),
				})
				return
			}
			
			c.JSON(http.StatusOK, AnalysisResponse{
				Status:    "success",
				Text:      req.Text,
				Result:    result,
				Timestamp: time.Now().UTC().Format(time.RFC3339),
			})
		})
		
		// Romanization endpoint
		api.POST("/romanize", func(c *gin.Context) {
			var req AnalysisRequest
			if err := c.ShouldBindJSON(&req); err != nil {
				c.JSON(http.StatusBadRequest, ErrorResponse{
					Status:  "error",
					Error:   "invalid_request",
					Message: err.Error(),
				})
				return
			}
			
			result, err := client.RomanizeText(req.Text)
			if err != nil {
				log.Printf("Romanization error: %v", err)
				c.JSON(http.StatusInternalServerError, ErrorResponse{
					Status:  "error",
					Error:   "romanization_failed",
					Message: err.Error(),
				})
				return
			}
			
			c.JSON(http.StatusOK, AnalysisResponse{
				Status:    "success",
				Text:      req.Text,
				Result:    result,
				Timestamp: time.Now().UTC().Format(time.RFC3339),
			})
		})
		
		// Kanji analysis endpoint
		api.POST("/kanji", func(c *gin.Context) {
			var req KanjiRequest
			if err := c.ShouldBindJSON(&req); err != nil {
				c.JSON(http.StatusBadRequest, ErrorResponse{
					Status:  "error",
					Error:   "invalid_request",
					Message: err.Error(),
				})
				return
			}
			
			result, err := client.AnalyzeKanji(req.Kanji)
			if err != nil {
				log.Printf("Kanji analysis error: %v", err)
				c.JSON(http.StatusInternalServerError, ErrorResponse{
					Status:  "error",
					Error:   "kanji_analysis_failed",
					Message: err.Error(),
				})
				return
			}
			
			c.JSON(http.StatusOK, AnalysisResponse{
				Status:    "success",
				Text:      req.Kanji,
				Result:    result,
				Timestamp: time.Now().UTC().Format(time.RFC3339),
			})
		})
		
		// Dictionary lookup endpoint
		api.POST("/dictionary", func(c *gin.Context) {
			var req AnalysisRequest
			if err := c.ShouldBindJSON(&req); err != nil {
				c.JSON(http.StatusBadRequest, ErrorResponse{
					Status:  "error",
					Error:   "invalid_request",
					Message: err.Error(),
				})
				return
			}
			
			// Dictionary lookup uses the same analysis function with detailed output
			result, err := client.AnalyzeText(req.Text)
			if err != nil {
				log.Printf("Dictionary lookup error: %v", err)
				c.JSON(http.StatusInternalServerError, ErrorResponse{
					Status:  "error",
					Error:   "dictionary_lookup_failed",
					Message: err.Error(),
				})
				return
			}
			
			c.JSON(http.StatusOK, AnalysisResponse{
				Status:    "success",
				Text:      req.Text,
				Result:    result,
				Timestamp: time.Now().UTC().Format(time.RFC3339),
			})
		})
	}
	
	// Start server
	port := os.Getenv("PORT")
	if port == "" {
		port = "8080"
	}
	
	log.Printf("Starting Go-Ichiran API server on port %s", port)
	log.Fatal(r.Run(":" + port))
}
