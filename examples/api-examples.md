# API Usage Examples

This document provides comprehensive examples of how to use the Go-Ichiran API server.

## Base URL

```
http://your-server:8080
```

For production with Nginx:
```
http://your-domain.com
```

## Authentication

Currently, the API does not require authentication. For production use, consider implementing API keys or other authentication mechanisms.

## Common Headers

```
Content-Type: application/json
Accept: application/json
```

## API Endpoints Examples

### 1. Health Check

**Request:**
```bash
curl -X GET http://localhost:8080/health
```

**Response:**
```json
{
    "status": "healthy",
    "service": "go-ichiran-api",
    "version": "1.0.0",
    "timestamp": "2025-06-22T18:52:49Z"
}
```

### 2. Text Analysis

Analyze Japanese text with complete segmentation and dictionary information.

**Request:**
```bash
curl -X POST http://localhost:8080/api/v1/analyze \
  -H "Content-Type: application/json" \
  -d '{
    "text": "一覧は最高だぞ"
  }'
```

**Response:**
```json
{
    "status": "success",
    "text": "一覧は最高だぞ",
    "result": {
        "raw_output": "ichiran wa saikō da zo\n(\"ichiran\" . \"一覧 【いちらん】\\n1. [n,vs] look; glance; sight; inspection\\n2. [n] summary; list; table; catalog; catalogue\")\n(\"wa\" . \"は\\n1. [prt] indicates sentence topic\")\n(\"saikō\" . \"最高 【さいこう】\\n1. [adj-no,adj-na,n] best; supreme; wonderful; finest\")\n(\"da\" . \"だ\\n1. [cop,cop-da] be; is\")\n(\"zo\" . \"ぞ\\n1. [prt] adds force or indicates command\")",
        "text": "一覧は最高だぞ"
    },
    "timestamp": "2025-06-22T18:52:49Z"
}
```

**More Examples:**

```bash
# Analyze a longer sentence
curl -X POST http://localhost:8080/api/v1/analyze \
  -H "Content-Type: application/json" \
  -d '{
    "text": "今日は天気がとても良いです。"
  }'

# Analyze with mixed scripts
curl -X POST http://localhost:8080/api/v1/analyze \
  -H "Content-Type: application/json" \
  -d '{
    "text": "私はプログラマーです。"
  }'
```

### 3. Romanization

Convert Japanese text to romaji (Latin script).

**Request:**
```bash
curl -X POST http://localhost:8080/api/v1/romanize \
  -H "Content-Type: application/json" \
  -d '{
    "text": "こんにちは世界"
  }'
```

**Response:**
```json
{
    "status": "success",
    "text": "こんにちは世界",
    "result": {
        "original": "こんにちは世界",
        "romaji": "konnichiwa sekai"
    },
    "timestamp": "2025-06-22T18:52:49Z"
}
```

**More Examples:**

```bash
# Romanize with kanji
curl -X POST http://localhost:8080/api/v1/romanize \
  -H "Content-Type: application/json" \
  -d '{
    "text": "東京駅"
  }'

# Romanize with complex sentence
curl -X POST http://localhost:8080/api/v1/romanize \
  -H "Content-Type: application/json" \
  -d '{
    "text": "ありがとうございました。"
  }'
```

### 4. Kanji Analysis

Get detailed information about individual kanji characters.

**Request:**
```bash
curl -X POST http://localhost:8080/api/v1/kanji \
  -H "Content-Type: application/json" \
  -d '{
    "kanji": "漢"
  }'
```

**Response:**
```json
{
    "status": "success",
    "text": "漢",
    "result": {
        "kanji": "漢",
        "analysis": "漢 [kan] - Chinese character, Han Chinese\nReadings: カン, おとこ\nMeaning: Chinese character, Han dynasty, man"
    },
    "timestamp": "2025-06-22T18:52:49Z"
}
```

**More Examples:**

```bash
# Common kanji
curl -X POST http://localhost:8080/api/v1/kanji \
  -H "Content-Type: application/json" \
  -d '{"kanji": "学"}'

curl -X POST http://localhost:8080/api/v1/kanji \
  -H "Content-Type: application/json" \
  -d '{"kanji": "日"}'

curl -X POST http://localhost:8080/api/v1/kanji \
  -H "Content-Type: application/json" \
  -d '{"kanji": "本"}'
```

### 5. Dictionary Lookup

Search for words in the Japanese dictionary.

**Request:**
```bash
curl -X POST http://localhost:8080/api/v1/dictionary \
  -H "Content-Type: application/json" \
  -d '{
    "text": "学校"
  }'
```

**Response:**
```json
{
    "status": "success",
    "text": "学校",
    "result": {
        "raw_output": "gakkō\n(\"gakkō\" . \"学校 【がっこう】\\n1. [n] school\")",
        "text": "学校"
    },
    "timestamp": "2025-06-22T18:52:49Z"
}
```

**More Examples:**

```bash
# Look up verbs
curl -X POST http://localhost:8080/api/v1/dictionary \
  -H "Content-Type: application/json" \
  -d '{"text": "食べる"}'

# Look up adjectives
curl -X POST http://localhost:8080/api/v1/dictionary \
  -H "Content-Type: application/json" \
  -d '{"text": "美しい"}'
```

## Error Handling

### Error Response Format

```json
{
    "status": "error",
    "error": "error_code",
    "message": "Detailed error message"
}
```

### Common Error Codes

- `invalid_request`: Request body is malformed or missing required fields
- `analysis_failed`: Ichiran analysis failed
- `romanization_failed`: Romanization process failed
- `kanji_analysis_failed`: Kanji analysis failed
- `dictionary_lookup_failed`: Dictionary lookup failed

### Example Error Responses

**Missing Text Field:**
```bash
curl -X POST http://localhost:8080/api/v1/analyze \
  -H "Content-Type: application/json" \
  -d '{}'
```

Response:
```json
{
    "status": "error",
    "error": "invalid_request",
    "message": "Key: 'AnalysisRequest.Text' Error:Tag: 'required' Tag: 'required' Field: 'text' Tag: 'required'"
}
```

**Invalid JSON:**
```bash
curl -X POST http://localhost:8080/api/v1/analyze \
  -H "Content-Type: application/json" \
  -d 'invalid json'
```

## Code Examples

### JavaScript (Browser/Node.js)

```javascript
// Async function to analyze text
async function analyzeText(text) {
    try {
        const response = await fetch('http://localhost:8080/api/v1/analyze', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({ text: text })
        });
        
        const result = await response.json();
        
        if (result.status === 'success') {
            console.log('Analysis result:', result.result);
            return result.result;
        } else {
            console.error('Analysis failed:', result.message);
            return null;
        }
    } catch (error) {
        console.error('Request failed:', error);
        return null;
    }
}

// Usage
analyzeText('こんにちは').then(result => {
    console.log(result);
});
```

### Python

```python
import requests
import json

class IchiranAPI:
    def __init__(self, base_url="http://localhost:8080"):
        self.base_url = base_url
    
    def analyze_text(self, text):
        """Analyze Japanese text"""
        response = requests.post(
            f"{self.base_url}/api/v1/analyze",
            headers={"Content-Type": "application/json"},
            json={"text": text}
        )
        return response.json()
    
    def romanize_text(self, text):
        """Romanize Japanese text"""
        response = requests.post(
            f"{self.base_url}/api/v1/romanize",
            headers={"Content-Type": "application/json"},
            json={"text": text}
        )
        return response.json()
    
    def analyze_kanji(self, kanji):
        """Analyze kanji character"""
        response = requests.post(
            f"{self.base_url}/api/v1/kanji",
            headers={"Content-Type": "application/json"},
            json={"kanji": kanji}
        )
        return response.json()

# Usage
api = IchiranAPI()

# Analyze text
result = api.analyze_text("一覧は最高だぞ")
print(json.dumps(result, indent=2, ensure_ascii=False))

# Romanize text
result = api.romanize_text("こんにちは")
print(json.dumps(result, indent=2, ensure_ascii=False))

# Analyze kanji
result = api.analyze_kanji("漢")
print(json.dumps(result, indent=2, ensure_ascii=False))
```

### Go

```go
package main

import (
    "bytes"
    "encoding/json"
    "fmt"
    "net/http"
)

type APIRequest struct {
    Text string `json:"text"`
}

type APIResponse struct {
    Status    string      `json:"status"`
    Text      string      `json:"text"`
    Result    interface{} `json:"result"`
    Timestamp string      `json:"timestamp"`
}

func analyzeText(baseURL, text string) (*APIResponse, error) {
    reqBody := APIRequest{Text: text}
    jsonData, err := json.Marshal(reqBody)
    if err != nil {
        return nil, err
    }

    resp, err := http.Post(
        baseURL+"/api/v1/analyze",
        "application/json",
        bytes.NewBuffer(jsonData),
    )
    if err != nil {
        return nil, err
    }
    defer resp.Body.Close()

    var result APIResponse
    err = json.NewDecoder(resp.Body).Decode(&result)
    if err != nil {
        return nil, err
    }

    return &result, nil
}

func main() {
    result, err := analyzeText("http://localhost:8080", "こんにちは")
    if err != nil {
        fmt.Printf("Error: %v\n", err)
        return
    }
    
    fmt.Printf("Result: %+v\n", result)
}
```

### Shell/Bash Scripts

```bash
#!/bin/bash

# Configuration
API_BASE="http://localhost:8080"

# Function to analyze text
analyze_text() {
    local text="$1"
    curl -s -X POST "${API_BASE}/api/v1/analyze" \
        -H "Content-Type: application/json" \
        -d "{\"text\": \"${text}\"}" | jq .
}

# Function to romanize text
romanize_text() {
    local text="$1"
    curl -s -X POST "${API_BASE}/api/v1/romanize" \
        -H "Content-Type: application/json" \
        -d "{\"text\": \"${text}\"}" | jq .
}

# Function to analyze kanji
analyze_kanji() {
    local kanji="$1"
    curl -s -X POST "${API_BASE}/api/v1/kanji" \
        -H "Content-Type: application/json" \
        -d "{\"kanji\": \"${kanji}\"}" | jq .
}

# Usage examples
echo "Analyzing text..."
analyze_text "一覧は最高だぞ"

echo -e "\nRomanizing text..."
romanize_text "こんにちは"

echo -e "\nAnalyzing kanji..."
analyze_kanji "漢"
```

## Performance and Rate Limiting

### Rate Limiting

The default Nginx configuration includes rate limiting:
- 10 requests per second per IP
- Burst of 20 requests
- Additional requests are delayed, not dropped

### Performance Tips

1. **Batch Processing**: For multiple texts, make parallel requests
2. **Caching**: Cache results for commonly analyzed text
3. **Connection Pooling**: Use HTTP connection pooling for multiple requests

### Monitoring Performance

```bash
# Check API response time
time curl -X POST http://localhost:8080/api/v1/analyze \
  -H "Content-Type: application/json" \
  -d '{"text": "テスト"}'

# Monitor with multiple requests
for i in {1..10}; do
    curl -X POST http://localhost:8080/api/v1/analyze \
        -H "Content-Type: application/json" \
        -d '{"text": "テスト'$i'"}' &
done
wait
```

## Production Considerations

### Security

1. **API Keys**: Implement authentication for production use
2. **HTTPS**: Always use SSL/TLS in production
3. **Input Validation**: Limit text length and validate input
4. **Rate Limiting**: Adjust rate limits based on your needs

### Monitoring

1. **Health Checks**: Monitor `/health` endpoint
2. **Logs**: Centralize log collection
3. **Metrics**: Monitor response times and error rates
4. **Alerts**: Set up alerts for failures

### Scaling

1. **Horizontal Scaling**: Add more API containers
2. **Load Balancing**: Use multiple API instances
3. **Database Optimization**: Optimize PostgreSQL for read performance
4. **Caching**: Add Redis caching layer

---

For more examples and advanced usage, see the main README.md file.
