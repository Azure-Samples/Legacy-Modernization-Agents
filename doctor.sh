#!/bin/bash

# COBOL Migration Tool - All-in-One Management Script
# ===================================================
# This script consolidates all functionality for setup, testing, running, and diagnostics

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Function to show usage
show_usage() {
    echo -e "${BOLD}${BLUE}🧠 COBOL to Java Quarkus Migration Tool${NC}"
    echo -e "${BLUE}==========================================${NC}"
    echo
    echo -e "${BOLD}Usage:${NC} $0 [command]"
    echo
    echo -e "${BOLD}Available Commands:${NC}"
    echo -e "  ${GREEN}setup${NC}           Interactive configuration setup"
    echo -e "  ${GREEN}test${NC}            Full system validation and testing"
    echo -e "  ${GREEN}run${NC}             Start the migration process"
    echo -e "  ${GREEN}doctor${NC}          Diagnose configuration issues (default)"
    echo -e "  ${GREEN}resume${NC}          Resume interrupted migration"
    echo -e "  ${GREEN}monitor${NC}         Monitor migration progress"
    echo -e "  ${GREEN}chat-test${NC}       Test chat logging functionality"
    echo -e "  ${GREEN}validate${NC}        Validate system requirements"
    echo -e "  ${GREEN}conversation${NC}    Start interactive conversation mode"
    echo
    echo -e "${BOLD}Examples:${NC}"
    echo -e "  $0              ${CYAN}# Run configuration doctor${NC}"
    echo -e "  $0 setup        ${CYAN}# Interactive setup${NC}"
    echo -e "  $0 test         ${CYAN}# Test configuration and dependencies${NC}"
    echo -e "  $0 run          ${CYAN}# Start migration${NC}"
    echo
}

# Function to load configuration
load_configuration() {
    if [[ -f "$SCRIPT_DIR/Config/load-config.sh" ]]; then
        source "$SCRIPT_DIR/Config/load-config.sh"
        return $?
    else
        echo -e "${RED}❌ Configuration loader not found: Config/load-config.sh${NC}"
        return 1
    fi
}

# Function for configuration doctor (original functionality)
run_doctor() {
    echo -e "${BLUE}🏥 Configuration Doctor - COBOL Migration Tool${NC}"
    echo "=============================================="
    echo

    # Check if configuration files exist
    echo -e "${BLUE}📋 Checking Configuration Files...${NC}"
    echo

    config_files_ok=true

    # Check template configuration
    if [[ -f "$SCRIPT_DIR/Config/ai-config.env" ]]; then
        echo -e "${GREEN}✅ Template configuration found: Config/ai-config.env${NC}"
    else
        echo -e "${RED}❌ Missing template configuration: Config/ai-config.env${NC}"
        config_files_ok=false
    fi

    # Check local configuration
    if [[ -f "$SCRIPT_DIR/Config/ai-config.local.env" ]]; then
        echo -e "${GREEN}✅ Local configuration found: Config/ai-config.local.env${NC}"
        local_config_exists=true
    else
        echo -e "${YELLOW}⚠️  Missing local configuration: Config/ai-config.local.env${NC}"
        local_config_exists=false
    fi

    # Check configuration loader
    if [[ -f "$SCRIPT_DIR/Config/load-config.sh" ]]; then
        echo -e "${GREEN}✅ Configuration loader found: Config/load-config.sh${NC}"
    else
        echo -e "${RED}❌ Missing configuration loader: Config/load-config.sh${NC}"
        config_files_ok=false
    fi

    # Check appsettings.json
    if [[ -f "$SCRIPT_DIR/Config/appsettings.json" ]]; then
        echo -e "${GREEN}✅ Application settings found: Config/appsettings.json${NC}"
    else
        echo -e "${RED}❌ Missing application settings: Config/appsettings.json${NC}"
        config_files_ok=false
    fi

    echo

    # If local config doesn't exist, offer to create it
    if [[ "$local_config_exists" == false ]]; then
        echo -e "${YELLOW}🔧 Local Configuration Setup${NC}"
        echo "----------------------------"
        echo "You need a local configuration file with your Azure OpenAI credentials."
        echo
        read -p "Would you like me to create Config/ai-config.local.env from the template? (y/n): " create_local
        
        if [[ "$create_local" =~ ^[Yy]$ ]]; then
            if [[ -f "$SCRIPT_DIR/Config/ai-config.local.env.template" ]]; then
                cp "$SCRIPT_DIR/Config/ai-config.local.env.template" "$SCRIPT_DIR/Config/ai-config.local.env"
                echo -e "${GREEN}✅ Created Config/ai-config.local.env from template${NC}"
                echo -e "${YELLOW}⚠️  You must edit this file with your actual Azure OpenAI credentials before running the migration tool.${NC}"
                local_config_exists=true
            else
                echo -e "${RED}❌ Template file not found: Config/ai-config.local.env.template${NC}"
            fi
        fi
        echo
    fi

    # Load and validate configuration if local config exists
    if [[ "$local_config_exists" == true ]]; then
        echo -e "${BLUE}🔍 Validating Configuration Content...${NC}"
        echo
        
        # Source the configuration loader
        if load_configuration && load_ai_config 2>/dev/null; then
            
            # Check required variables
            required_vars=(
                "AZURE_OPENAI_ENDPOINT"
                "AZURE_OPENAI_API_KEY"
                "AZURE_OPENAI_DEPLOYMENT_NAME"
                "AZURE_OPENAI_MODEL_ID"
            )
            
            config_valid=true
            
            for var in "${required_vars[@]}"; do
                value="${!var}"
                if [[ -z "$value" ]]; then
                    echo -e "${RED}❌ Missing: $var${NC}"
                    config_valid=false
                elif [[ "$value" == *"your-"* ]]; then
                    echo -e "${YELLOW}⚠️  Template placeholder detected in $var: $value${NC}"
                    config_valid=false
                else
                    # Mask API key for display
                    if [[ "$var" == "AZURE_OPENAI_API_KEY" ]]; then
                        masked_value="${value:0:8}...${value: -4}"
                        echo -e "${GREEN}✅ $var: $masked_value${NC}"
                    else
                        echo -e "${GREEN}✅ $var: $value${NC}"
                    fi
                fi
            done
            
            echo
            
            if [[ "$config_valid" == true ]]; then
                echo -e "${GREEN}🎉 Configuration validation successful!${NC}"
                echo
                echo "Your configuration is ready to use. You can now run:"
                echo "  ./doctor.sh run"
                echo "  ./doctor.sh test"
                echo "  dotnet run"
            else
                echo -e "${YELLOW}⚠️  Configuration needs attention${NC}"
                echo
                echo "Next steps:"
                echo "1. Edit Config/ai-config.local.env"
                echo "2. Replace template placeholders with your actual Azure OpenAI credentials"
                echo "3. Run this doctor script again to validate"
                echo
                echo "Need help? Run: ./doctor.sh setup"
            fi
        else
            echo -e "${RED}❌ Failed to load configuration${NC}"
        fi
    fi

    echo
    echo -e "${BLUE}🔧 Available Commands${NC}"
    echo "===================="
    echo "• ./doctor.sh setup - Interactive configuration setup"
    echo "• ./doctor.sh test - Full system validation"
    echo "• ./doctor.sh run - Start migration"
    echo "• CONFIGURATION_GUIDE.md - Detailed setup instructions"

    echo
    echo -e "${BLUE}💡 Troubleshooting Tips${NC}"
    echo "======================"
    echo "• Make sure your Azure OpenAI resource is deployed and accessible"
    echo "• Verify your model deployment names match your Azure setup"
    echo "• Check that your API key has proper permissions"
    echo "• Ensure your endpoint URL is correct (should end with /)"

    echo
    echo "Configuration doctor completed!"
}

# Function for interactive setup
run_setup() {
    echo -e "${CYAN}🚀 COBOL to Java Migration Tool - Setup${NC}"
    echo "========================================"
    echo ""

    # Check if local config already exists
    LOCAL_CONFIG="$SCRIPT_DIR/Config/ai-config.local.env"
    if [ -f "$LOCAL_CONFIG" ]; then
        echo -e "${YELLOW}⚠️  Local configuration already exists:${NC} $LOCAL_CONFIG"
        echo ""
        read -p "Do you want to overwrite it? (y/N): " -n 1 -r
        echo ""
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            echo -e "${BLUE}ℹ️  Setup cancelled. Your existing configuration is preserved.${NC}"
            return 0
        fi
    fi

    # Create local config from template
    echo -e "${BLUE}📁 Creating local configuration file...${NC}"
    TEMPLATE_CONFIG="$SCRIPT_DIR/Config/ai-config.local.env.template"

    if [ ! -f "$TEMPLATE_CONFIG" ]; then
        echo -e "${RED}❌ Template configuration file not found: $TEMPLATE_CONFIG${NC}"
        return 1
    fi

    cp "$TEMPLATE_CONFIG" "$LOCAL_CONFIG"
    echo -e "${GREEN}✅ Created: $LOCAL_CONFIG${NC}"
    echo ""

    # Interactive configuration
    echo -e "${BLUE}🔧 Interactive Configuration Setup${NC}"
    echo "=================================="
    echo ""
    echo "Please provide your Azure OpenAI configuration details:"
    echo ""

    # Get Azure OpenAI Endpoint
    read -p "Azure OpenAI Endpoint (e.g., https://your-resource.openai.azure.com/): " endpoint
    if [[ -n "$endpoint" ]]; then
        # Ensure endpoint ends with /
        [[ "${endpoint}" != */ ]] && endpoint="${endpoint}/"
        sed -i.bak "s|AZURE_OPENAI_ENDPOINT=\".*\"|AZURE_OPENAI_ENDPOINT=\"$endpoint\"|" "$LOCAL_CONFIG"
    fi

    # Get API Key
    read -s -p "Azure OpenAI API Key: " api_key
    echo ""
    if [[ -n "$api_key" ]]; then
        sed -i.bak "s|AZURE_OPENAI_API_KEY=\".*\"|AZURE_OPENAI_API_KEY=\"$api_key\"|" "$LOCAL_CONFIG"
    fi

    # Get Model Deployment Name
    read -p "Model Deployment Name (default: gpt-4.1): " deployment_name
    deployment_name=${deployment_name:-gpt-4.1}
    sed -i.bak "s|AZURE_OPENAI_DEPLOYMENT_NAME=\".*\"|AZURE_OPENAI_DEPLOYMENT_NAME=\"$deployment_name\"|" "$LOCAL_CONFIG"

    # Update model ID to match deployment name
    sed -i.bak "s|AZURE_OPENAI_MODEL_ID=\".*\"|AZURE_OPENAI_MODEL_ID=\"$deployment_name\"|" "$LOCAL_CONFIG"

    # Clean up backup file
    rm -f "$LOCAL_CONFIG.bak"

    echo ""
    echo -e "${GREEN}✅ Configuration completed!${NC}"
    echo ""
    echo -e "${BLUE}🔍 Testing configuration...${NC}"
    
    # Test the configuration
    if load_configuration && load_ai_config 2>/dev/null; then
        echo -e "${GREEN}✅ Configuration loaded successfully!${NC}"
        echo ""
        echo -e "${BLUE}Next steps:${NC}"
        echo "1. Run: ./doctor.sh test    # Test system dependencies"
        echo "2. Run: ./doctor.sh run     # Start migration"
        echo ""
        echo "Your configuration is ready to use!"
    else
        echo -e "${RED}❌ Configuration test failed${NC}"
        echo "Please check your settings and try again."
    fi
}

# Function for comprehensive testing
run_test() {
    echo -e "${BOLD}${BLUE}COBOL to Java Quarkus Migration Tool - Test Suite${NC}"
    echo "=================================================="

    # Load configuration
    echo "🔧 Loading AI configuration..."
    if ! load_configuration; then
        echo -e "${RED}❌ Failed to load configuration system${NC}"
        return 1
    fi

    echo ""
    echo "Testing Configuration:"
    echo "====================="

    if load_ai_config; then
        echo ""
        echo -e "${GREEN}✅ Configuration loaded successfully!${NC}"
        echo ""
        echo "Configuration Summary:"
        show_config_summary 2>/dev/null || echo "Configuration details loaded"
    else
        echo ""
        echo -e "${RED}❌ Configuration loading failed!${NC}"
        echo ""
        echo "To fix this:"
        echo "1. Run: ./doctor.sh setup"
        echo "2. Edit Config/ai-config.local.env with your Azure OpenAI credentials"
        echo "3. Run this test again"
        return 1
    fi

    # Check .NET version
    echo ""
    echo "Checking .NET version..."
    dotnet_version=$(dotnet --version 2>/dev/null)
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✅ .NET version: $dotnet_version${NC}"
        
        # Check if it's .NET 8.0 or higher
        major_version=$(echo $dotnet_version | cut -d. -f1)
        if [ "$major_version" -ge 8 ]; then
            echo -e "${GREEN}✅ .NET 8.0+ requirement satisfied${NC}"
        else
            echo -e "${YELLOW}⚠️  Warning: .NET 8.0+ recommended (current: $dotnet_version)${NC}"
        fi
    else
        echo -e "${RED}❌ .NET is not installed or not in PATH${NC}"
        return 1
    fi

    # Check Semantic Kernel dependencies
    echo ""
    echo "Checking Semantic Kernel dependencies..."
    if dotnet list package | grep -q "Microsoft.SemanticKernel"; then
        sk_version=$(dotnet list package | grep "Microsoft.SemanticKernel" | awk '{print $3}' | head -1)
        echo -e "${GREEN}✅ Semantic Kernel dependencies resolved (version: $sk_version)${NC}"
    else
        echo -e "${YELLOW}⚠️  Semantic Kernel packages not found, checking project file...${NC}"
    fi

    # Build project
    echo ""
    echo "Building project and restoring packages..."
    echo "="
    if timeout 30s dotnet build --no-restore --verbosity quiet 2>/dev/null || dotnet build --verbosity minimal; then
        echo -e "${GREEN}✅ Project builds successfully${NC}"
    else
        echo -e "${RED}❌ Project build failed${NC}"
        echo "Try running: dotnet restore"
        return 1
    fi

    # Check source folders
    echo ""
    echo "Checking source folders..."
    cobol_files=$(find "$SCRIPT_DIR/cobol-source" -name "*.cbl" -o -name "*.cpy" 2>/dev/null | wc -l)
    if [ "$cobol_files" -gt 0 ]; then
        echo -e "${GREEN}✅ Found $(printf "%8d" $cobol_files) COBOL files in cobol-source directory${NC}"
    else
        echo -e "${YELLOW}⚠️  No COBOL files found in cobol-source directory${NC}"
        echo "   Add your COBOL files to ./cobol-source/ to test migration"
    fi

    # Check output directories
    echo ""
    echo "Checking output directories..."
    if [ -d "$SCRIPT_DIR/java-output" ]; then
        java_files=$(find "$SCRIPT_DIR/java-output" -name "*.java" 2>/dev/null | wc -l)
        if [ "$java_files" -gt 0 ]; then
            echo -e "${GREEN}✅ Found previous Java output ($java_files files)${NC}"
        else
            echo -e "${BLUE}ℹ️  No previous Java output found (will be created during migration)${NC}"
        fi
    else
        echo -e "${BLUE}ℹ️  Output directory will be created during migration${NC}"
    fi

    # Check logging infrastructure
    echo ""
    echo "Checking logging infrastructure..."
    if [ -d "$SCRIPT_DIR/Logs" ]; then
        log_files=$(find "$SCRIPT_DIR/Logs" -name "*.log" 2>/dev/null | wc -l)
        echo -e "${GREEN}✅ Log directory exists with $(printf "%8d" $log_files) log files${NC}"
    else
        mkdir -p "$SCRIPT_DIR/Logs"
        echo -e "${GREEN}✅ Created Logs directory${NC}"
    fi

    echo ""
    echo -e "${GREEN}🚀 Ready to run migration!${NC}"
    echo ""
    echo "Migration Options:"
    echo "  Quick Start:  ./doctor.sh run"
    echo "  Manual:       dotnet run -- --cobol-source ./cobol-source --java-output ./java-output --verbose"
    echo ""
    if [ "$cobol_files" -gt 0 ]; then
        echo "Expected Results:"
        echo "  - Process $cobol_files COBOL files"
        echo "  - Generate $cobol_files+ Java files"
        echo "  - Create dependency maps"
        echo "  - Generate migration reports"
    fi
}

# Function to run migration
run_migration() {
    echo -e "${BLUE}🚀 Starting COBOL to Java Quarkus Migration...${NC}"
    echo "=============================================="

    # Load configuration
    echo "🔧 Loading AI configuration..."
    if ! load_configuration; then
        echo -e "${RED}❌ Configuration loading failed. Please run: ./doctor.sh setup${NC}"
        return 1
    fi

    # Load and validate configuration
    if ! load_ai_config; then
        echo -e "${RED}❌ Configuration loading failed. Please check your ai-config.local.env file.${NC}"
        return 1
    fi

    echo ""
    echo "🚀 Starting COBOL to Java Quarkus Migration..."
    echo "=============================================="

    # Run the application with updated folder structure
    dotnet run -- --cobol-source ./cobol-source --java-output ./java-output
}

# Function to resume migration
run_resume() {
    echo -e "${BLUE}🔄 Resuming COBOL to Java Migration...${NC}"
    echo "======================================"

    # Load configuration
    if ! load_configuration || ! load_ai_config; then
        echo -e "${RED}❌ Configuration loading failed. Please check your setup.${NC}"
        return 1
    fi

    echo ""
    echo "Checking for resumable migration state..."
    
    # Check for existing partial results
    if [ -d "$SCRIPT_DIR/java-output" ] && [ "$(ls -A $SCRIPT_DIR/java-output 2>/dev/null)" ]; then
        echo -e "${GREEN}✅ Found existing migration output${NC}"
        echo "Resuming from last position..."
    else
        echo -e "${YELLOW}⚠️  No previous migration state found${NC}"
        echo "Starting fresh migration..."
    fi

    # Run with resume logic
    dotnet run -- --cobol-source ./cobol-source --java-output ./java-output --resume
}

# Function to monitor migration
run_monitor() {
    echo -e "${BLUE}📊 Migration Progress Monitor${NC}"
    echo "============================"

    if [ ! -d "$SCRIPT_DIR/Logs" ]; then
        echo -e "${YELLOW}⚠️  No logs directory found${NC}"
        return 1
    fi

    echo "Monitoring migration logs..."
    echo "Press Ctrl+C to exit monitoring"
    echo ""

    # Monitor log files for progress
    tail -f "$SCRIPT_DIR/Logs"/*.log 2>/dev/null || echo "No active log files found"
}

# Function to test chat logging
run_chat_test() {
    echo -e "${BLUE}💬 Testing Chat Logging Functionality${NC}"
    echo "====================================="

    # Load configuration
    if ! load_configuration || ! load_ai_config; then
        echo -e "${RED}❌ Configuration loading failed.${NC}"
        return 1
    fi

    echo "Testing chat logging system..."
    
    # Run a simple test
    dotnet run -- --test-chat-logging
}

# Function to validate system
run_validate() {
    echo -e "${BLUE}✅ System Validation${NC}"
    echo "==================="

    errors=0

    # Check .NET
    if command -v dotnet >/dev/null 2>&1; then
        echo -e "${GREEN}✅ .NET CLI available${NC}"
    else
        echo -e "${RED}❌ .NET CLI not found${NC}"
        ((errors++))
    fi

    # Check configuration files
    required_files=(
        "Config/ai-config.env"
        "Config/load-config.sh"
        "Config/appsettings.json"
        "CobolToQuarkusMigration.csproj"
        "Program.cs"
    )

    for file in "${required_files[@]}"; do
        if [ -f "$SCRIPT_DIR/$file" ]; then
            echo -e "${GREEN}✅ $file${NC}"
        else
            echo -e "${RED}❌ Missing: $file${NC}"
            ((errors++))
        fi
    done

    # Check directories
    for dir in "cobol-source" "java-output"; do
        if [ -d "$SCRIPT_DIR/$dir" ]; then
            echo -e "${GREEN}✅ Directory: $dir${NC}"
        else
            echo -e "${YELLOW}⚠️  Creating directory: $dir${NC}"
            mkdir -p "$SCRIPT_DIR/$dir"
        fi
    done

    if [ $errors -eq 0 ]; then
        echo -e "${GREEN}🎉 System validation passed!${NC}"
        return 0
    else
        echo -e "${RED}❌ System validation failed with $errors errors${NC}"
        return 1
    fi
}

# Function for conversation mode
run_conversation() {
    echo -e "${BLUE}💭 Interactive Conversation Mode${NC}"
    echo "================================"
    
    # Load configuration
    if ! load_configuration || ! load_ai_config; then
        echo -e "${RED}❌ Configuration loading failed.${NC}"
        return 1
    fi

    echo "Starting interactive conversation with the migration system..."
    echo "Type 'exit' to quit"
    echo ""

    dotnet run -- --interactive
}

# Main command routing
main() {
    # Create required directories if they don't exist
    mkdir -p "$SCRIPT_DIR/cobol-source" "$SCRIPT_DIR/java-output" "$SCRIPT_DIR/Logs"

    case "${1:-doctor}" in
        "setup")
            run_setup
            ;;
        "test")
            run_test
            ;;
        "run")
            run_migration
            ;;
        "doctor"|"")
            run_doctor
            ;;
        "resume")
            run_resume
            ;;
        "monitor")
            run_monitor
            ;;
        "chat-test")
            run_chat_test
            ;;
        "validate")
            run_validate
            ;;
        "conversation")
            run_conversation
            ;;
        "help"|"-h"|"--help")
            show_usage
            ;;
        *)
            echo -e "${RED}❌ Unknown command: $1${NC}"
            echo ""
            show_usage
            exit 1
            ;;
    esac
}

# Run main function with all arguments
main "$@"
