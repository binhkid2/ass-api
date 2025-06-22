#!/bin/bash

# Test script for Go-Ichiran API Server
# This script tests all API endpoints to verify proper functionality

set -e

# Configuration
API_BASE="${API_BASE:-http://localhost:8080}"
TIMEOUT=30

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test results
TESTS_PASSED=0
TESTS_FAILED=0
TOTAL_TESTS=0

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[PASS]${NC} $1"
    ((TESTS_PASSED++))
}

log_error() {
    echo -e "${RED}[FAIL]${NC} $1"
    ((TESTS_FAILED++))
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

# Function to make API request
make_request() {
    local method="$1"
    local endpoint="$2"
    local data="$3"
    local expected_status="$4"
    
    if [ -n "$data" ]; then
        response=$(curl -s -w "HTTPSTATUS:%{http_code}" \
            -X "$method" \
            -H "Content-Type: application/json" \
            -d "$data" \
            --max-time "$TIMEOUT" \
            "$API_BASE$endpoint" || echo "HTTPSTATUS:000")
    else
        response=$(curl -s -w "HTTPSTATUS:%{http_code}" \
            -X "$method" \
            --max-time "$TIMEOUT" \
            "$API_BASE$endpoint" || echo "HTTPSTATUS:000")
    fi
    
    http_status=$(echo "$response" | grep -o "HTTPSTATUS:[0-9]*" | cut -d: -f2)
    body=$(echo "$response" | sed '/HTTPSTATUS:/d')
    
    if [ "$http_status" = "$expected_status" ]; then
        return 0
    else
        echo "Expected status $expected_status, got $http_status"
        echo "Response: $body"
        return 1
    fi
}

# Function to test JSON response structure
test_json_structure() {
    local response="$1"
    local required_fields="$2"
    
    for field in $required_fields; do
        if ! echo "$response" | jq -e ".$field" > /dev/null 2>&1; then
            echo "Missing required field: $field"
            return 1
        fi
    done
    return 0
}

echo "=== Go-Ichiran API Test Suite ==="
echo "Testing API at: $API_BASE"
echo "Timeout: ${TIMEOUT}s"
echo ""

# Test 1: Health Check
log_info "Test 1: Health Check"
((TOTAL_TESTS++))
if make_request "GET" "/health" "" "200"; then
    response_body=$(curl -s "$API_BASE/health")
    if test_json_structure "$response_body" "status service version timestamp"; then
        status=$(echo "$response_body" | jq -r '.status')
        if [ "$status" = "healthy" ]; then
            log_success "Health check endpoint working correctly"
        else
            log_error "Health check returned status: $status"
        fi
    else
        log_error "Health check response missing required fields"
    fi
else
    log_error "Health check endpoint failed"
fi

# Test 2: Text Analysis
log_info "Test 2: Text Analysis"
((TOTAL_TESTS++))
test_data='{"text": "一覧は最高だぞ"}'
if make_request "POST" "/api/v1/analyze" "$test_data" "200"; then
    response_body=$(curl -s -X POST -H "Content-Type: application/json" -d "$test_data" "$API_BASE/api/v1/analyze")
    if test_json_structure "$response_body" "status text result timestamp"; then
        status=$(echo "$response_body" | jq -r '.status')
        if [ "$status" = "success" ]; then
            log_success "Text analysis endpoint working correctly"
        else
            log_error "Text analysis returned status: $status"
        fi
    else
        log_error "Text analysis response missing required fields"
    fi
else
    log_error "Text analysis endpoint failed"
fi

# Test 3: Romanization
log_info "Test 3: Romanization"
((TOTAL_TESTS++))
test_data='{"text": "こんにちは"}'
if make_request "POST" "/api/v1/romanize" "$test_data" "200"; then
    response_body=$(curl -s -X POST -H "Content-Type: application/json" -d "$test_data" "$API_BASE/api/v1/romanize")
    if test_json_structure "$response_body" "status text result timestamp"; then
        status=$(echo "$response_body" | jq -r '.status')
        if [ "$status" = "success" ]; then
            log_success "Romanization endpoint working correctly"
        else
            log_error "Romanization returned status: $status"
        fi
    else
        log_error "Romanization response missing required fields"
    fi
else
    log_error "Romanization endpoint failed"
fi

# Test 4: Kanji Analysis
log_info "Test 4: Kanji Analysis"
((TOTAL_TESTS++))
test_data='{"kanji": "漢"}'
if make_request "POST" "/api/v1/kanji" "$test_data" "200"; then
    response_body=$(curl -s -X POST -H "Content-Type: application/json" -d "$test_data" "$API_BASE/api/v1/kanji")
    if test_json_structure "$response_body" "status text result timestamp"; then
        status=$(echo "$response_body" | jq -r '.status')
        if [ "$status" = "success" ]; then
            log_success "Kanji analysis endpoint working correctly"
        else
            log_error "Kanji analysis returned status: $status"
        fi
    else
        log_error "Kanji analysis response missing required fields"
    fi
else
    log_error "Kanji analysis endpoint failed"
fi

# Test 5: Dictionary Lookup
log_info "Test 5: Dictionary Lookup"
((TOTAL_TESTS++))
test_data='{"text": "学校"}'
if make_request "POST" "/api/v1/dictionary" "$test_data" "200"; then
    response_body=$(curl -s -X POST -H "Content-Type: application/json" -d "$test_data" "$API_BASE/api/v1/dictionary")
    if test_json_structure "$response_body" "status text result timestamp"; then
        status=$(echo "$response_body" | jq -r '.status')
        if [ "$status" = "success" ]; then
            log_success "Dictionary lookup endpoint working correctly"
        else
            log_error "Dictionary lookup returned status: $status"
        fi
    else
        log_error "Dictionary lookup response missing required fields"
    fi
else
    log_error "Dictionary lookup endpoint failed"
fi

# Test 6: Error Handling (Invalid Request)
log_info "Test 6: Error Handling - Invalid Request"
((TOTAL_TESTS++))
test_data='{}'  # Missing required field
if make_request "POST" "/api/v1/analyze" "$test_data" "400"; then
    response_body=$(curl -s -X POST -H "Content-Type: application/json" -d "$test_data" "$API_BASE/api/v1/analyze")
    if test_json_structure "$response_body" "status error message"; then
        status=$(echo "$response_body" | jq -r '.status')
        if [ "$status" = "error" ]; then
            log_success "Error handling working correctly"
        else
            log_error "Error handling returned unexpected status: $status"
        fi
    else
        log_error "Error response missing required fields"
    fi
else
    log_error "Error handling test failed"
fi

# Test 7: Performance Test
log_info "Test 7: Performance Test (5 concurrent requests)"
((TOTAL_TESTS++))
start_time=$(date +%s)
for i in {1..5}; do
    (curl -s -X POST -H "Content-Type: application/json" \
        -d '{"text": "テスト'$i'"}' \
        "$API_BASE/api/v1/analyze" > /dev/null) &
done
wait
end_time=$(date +%s)
duration=$((end_time - start_time))

if [ $duration -lt 30 ]; then
    log_success "Performance test completed in ${duration}s (under 30s threshold)"
else
    log_warn "Performance test took ${duration}s (over 30s threshold)"
fi

echo ""
echo "=== Test Summary ==="
echo "Total Tests: $TOTAL_TESTS"
echo "Passed: $TESTS_PASSED"
echo "Failed: $TESTS_FAILED"

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    echo ""
    echo "=== API Endpoints Ready ==="
    echo "Health Check: $API_BASE/health"
    echo "Text Analysis: $API_BASE/api/v1/analyze"
    echo "Romanization: $API_BASE/api/v1/romanize"
    echo "Kanji Analysis: $API_BASE/api/v1/kanji"
    echo "Dictionary: $API_BASE/api/v1/dictionary"
    echo ""
    echo "Example usage:"
    echo "curl -X POST $API_BASE/api/v1/analyze \\"
    echo "  -H \"Content-Type: application/json\" \\"
    echo "  -d '{\"text\": \"こんにちは\"}'"
    exit 0
else
    echo -e "${RED}$TESTS_FAILED test(s) failed!${NC}"
    echo ""
    echo "=== Troubleshooting Tips ==="
    echo "1. Ensure all containers are running: docker-compose ps"
    echo "2. Check container logs: docker-compose logs"
    echo "3. Verify ichiran container is healthy: docker-compose ps ichiran"
    echo "4. Test ichiran directly: docker exec ichiran-container ichiran-cli --help"
    echo "5. Check API server logs: docker-compose logs api"
    exit 1
fi
