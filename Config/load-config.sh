#!/bin/bash

# =============================================================================
# Configuration Loader for COBOL Migration Tool
# =============================================================================
# This script loads AI configuration from centralized config files.
# It supports multiple configuration sources with priority order:
# 1. ai-config.local.env (local overrides - not in git)
# 2. ai-config.env (template/defaults)
# 3. Environment variables (if already set)
# =============================================================================

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_DIR="$SCRIPT_DIR"

# Configuration file paths
LOCAL_CONFIG="$CONFIG_DIR/ai-config.local.env"
TEMPLATE_CONFIG="$CONFIG_DIR/ai-config.env"

# Function to log messages
log_info() {
    echo -e "${BLUE}[CONFIG]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[CONFIG]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[CONFIG]${NC} $1"
}

log_error() {
    echo -e "${RED}[CONFIG]${NC} $1"
}

# Function to load environment file
load_env_file() {
    local file="$1"
    local description="$2"
    
    if [ -f "$file" ]; then
        log_info "Loading $description: $file"
        
        # Load the file, ignoring comments and empty lines
        while IFS= read -r line || [[ -n "$line" ]]; do
            # Skip comments and empty lines
            if [[ "$line" =~ ^[[:space:]]*# ]] || [[ -z "${line// }" ]]; then
                continue
            fi
            
            # Export the variable
            if [[ "$line" =~ ^[[:space:]]*([^=]+)=(.*)$ ]]; then
                local var_name="${BASH_REMATCH[1]// }"
                local var_value="${BASH_REMATCH[2]}"
                
                # Remove surrounding quotes if present
                var_value=$(echo "$var_value" | sed 's/^["'\'']//' | sed 's/["'\'']$//')
                
                # Only set if not already set (allows environment override)
                if [ -z "${!var_name}" ]; then
                    export "$var_name=$var_value"
                    log_info "Set $var_name"
                else
                    log_info "Skipped $var_name (already set)"
                fi
            fi
        done < "$file"
        return 0
    else
        log_warning "$description not found: $file"
        return 1
    fi
}

# Function to validate required configuration
validate_config() {
    local errors=0
    
    log_info "Validating configuration..."
    
    # Required variables
    local required_vars=(
        "AZURE_OPENAI_ENDPOINT"
        "AZURE_OPENAI_API_KEY"
        "AZURE_OPENAI_DEPLOYMENT_NAME"
        "AZURE_OPENAI_MODEL_ID"
    )
    
    for var in "${required_vars[@]}"; do
        if [ -z "${!var}" ]; then
            log_error "Required variable $var is not set"
            ((errors++))
        elif [[ "${!var}" == *"your-"* ]] || [[ "${!var}" == *"placeholder"* ]]; then
            log_error "Variable $var contains placeholder value: ${!var}"
            ((errors++))
        else
            log_success "✓ $var is configured"
        fi
    done
    
    return $errors
}

# Function to display configuration summary
show_config_summary() {
    log_info "Configuration Summary:"
    echo "  Endpoint: ${AZURE_OPENAI_ENDPOINT:-'NOT SET'}"
    echo "  Model ID: ${AZURE_OPENAI_MODEL_ID:-'NOT SET'}"
    echo "  Deployment: ${AZURE_OPENAI_DEPLOYMENT_NAME:-'NOT SET'}"
    echo "  API Key: ${AZURE_OPENAI_API_KEY:0:10}... (${#AZURE_OPENAI_API_KEY} chars)"
    echo "  Source Folder: ${COBOL_SOURCE_FOLDER:-'SampleCobol'}"
    echo "  Output Folder: ${JAVA_OUTPUT_FOLDER:-'JavaOutput'}"
}

# Function to create local config from template
create_local_config() {
    if [ ! -f "$LOCAL_CONFIG" ] && [ -f "$TEMPLATE_CONFIG" ]; then
        log_info "Creating local configuration file..."
        cp "$TEMPLATE_CONFIG" "$LOCAL_CONFIG"
        log_warning "Please edit $LOCAL_CONFIG with your actual Azure OpenAI credentials"
        log_warning "Remember to add ai-config.local.env to your .gitignore file"
        return 1
    fi
    return 0
}

# Main configuration loading function
load_ai_config() {
    local validate_only=${1:-false}
    
    log_info "Loading AI configuration..."
    
    # Create local config if it doesn't exist
    if ! create_local_config && [ "$validate_only" = "false" ]; then
        log_error "Local configuration needs to be updated before proceeding"
        return 1
    fi
    
    # Load configuration files in priority order (local overrides first)
    load_env_file "$LOCAL_CONFIG" "local configuration"
    
    # Then load template defaults for any remaining unset values
    load_env_file "$TEMPLATE_CONFIG" "template configuration"
    
    # 3. Validate configuration
    if ! validate_config; then
        log_error "Configuration validation failed"
        return 1
    fi
    
    # 4. Show summary
    show_config_summary
    
    log_success "Configuration loaded successfully"
    return 0
}

# Function to check if this script is being sourced or executed
check_execution_context() {
    if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
        # Script is being executed directly
        echo "Configuration Loader - COBOL Migration Tool"
        echo "==========================================="
        load_ai_config true
        exit $?
    fi
    # Script is being sourced - just load the functions
}

# Export functions for use in other scripts
export -f load_ai_config
export -f validate_config
export -f show_config_summary
export -f log_info
export -f log_success
export -f log_warning
export -f log_error

# Check execution context
check_execution_context
