# Sample Code

This directory contains sample COBOL code, guidelines related to the sample code, sample Java output, and sample IaC for running the code in Azure.

### Types of COBOL code

There are various types of COBOL code that could be the target for migration, for example:

- Trigger types: For example, scheduled jobs, event-driven execution (e.g., file arrivals, message arrivals, end-user transactions), operator invoked.
- COBOL dialect: For example, IBM Enterprise COBOL for z/OS, Micro Focus Visual COBOL, Fujitsu NetCOBOL, regional variants (e.g., Hitachi, NEC), GnuCOBOL (open-source).
- Amount of code: Migration estates range from a few hundred programs to many tens of thousands. Individual COBOL programs are often thousands of lines, with larger outliers. Total portfolios commonly reach many millions of lines.

## Hello World Sample (Printing "Hello, world!")

- Trigger type: Scheduled job
- COBOL dialect: GnuCOBOL
- Amount of code: One file with five lines of code

### Converting the sample code to Java
Recommend add the following to the prompt in JavaConverterAgent.cs
```
- Make sure to add a package declaration at the top of the Java files to match its folder path
- Make it runnable as a sandalone Java application
- Convert COBOL `DISPLAY` to Java logging using a standard logging framework
- Assume that the Java code will be run as a schedule job as opposed to a web app or a REST API
```

### Running the Java code locally
1. Set up Maven
  - Create pom.xml
  - Test and iterate with `mvn -q quarkus:run`
2. Run in Docker
  - Create `Dockerfile`
  - `docker build -t hello-quarkus .`
  - `docker run --rm hello-quarkus`

### Running the Java code on Azure
1. Provision Azure Container Registry, Azure Container Apps, Azure Log Analytics. There is sample Bicep in the  `infra/` directory.
2. Build container and push the image to Azure Container Registry (e.g., using `az acr build`)
3. Have the Azure Container Apps pull the image from the Azure Container Registry
4. View Log Analytics to see "Hello, world!" being logged
