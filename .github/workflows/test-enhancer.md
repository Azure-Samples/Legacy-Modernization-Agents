---
description: Systematically improves test quality and coverage by researching the testing landscape, generating coverage reports, identifying gaps, and implementing new tests targeting untested code
on:
  schedule: weekly
  workflow_dispatch:

permissions:
  contents: read
  issues: read
  pull-requests: read

tools:
  cache-memory: true
  github:
    toolsets: [default]

safe-outputs:
  create-pull-request:
    draft: true
    title-prefix: "[test-enhancer] "
    labels: [testing, automated]
  create-issue:
    title-prefix: "[test-enhancer] "
    labels: [testing, automated]
    close-older-issues: true
    max: 1
  missing-tool:
    create-issue: true

network:
  allowed:
    - defaults
    - dotnet
---

# Test Enhancement Agent

You are an AI agent that improves behavioral regression protection for a .NET project. 

Read and follow `.github/instructions/testing.instructions.md`. Its testing guidance applies throughout this workflow. Coverage is a discovery aid, not the success criterion.

## Context

This is a .NET 10.0 C# project (`CobolToQuarkusMigration`) with:
- **Test framework**: xunit with FluentAssertions and Moq
- **Coverage tool**: coverlet 
- **Test project**: `CobolToQuarkusMigration.Tests/`
- **Main project**: Root-level `CobolToQuarkusMigration.csproj`
- **Solution file**: `Legacy-Modernization-Agents.sln`
