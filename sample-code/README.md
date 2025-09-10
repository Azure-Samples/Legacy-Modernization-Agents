# Sample Code

This directory contains sample COBOL code, guidelines related to the sample code, sample Java output, and sample Infrastructure as Code (IaC) for running the code in Azure.

## Types of COBOL Code

There are various types of COBOL code that could be targets for migration, for example:

- **Trigger types**: For example, scheduled jobs, event-driven execution (e.g., file arrivals, message arrivals, end-user transactions), or operator-invoked processes.
- **COBOL dialect**: For example, IBM Enterprise COBOL for z/OS, Micro Focus Visual COBOL, Fujitsu NetCOBOL, regional variants (e.g., Hitachi, NEC), or GnuCOBOL (open-source).
- **Amount of code**: Migration estates range from a few hundred programs to tens of thousands. Individual COBOL programs are often thousands of lines long, with larger outliers. Total portfolios commonly reach millions of lines.

## Hello World Sample (Printing "Hello, world!")

- **Trigger type**: Scheduled job
- **COBOL dialect**: GnuCOBOL
- **Amount of code**: One file with five lines of code

> Note: This Hello World sample is intentionally minimal — it's provided so you can try going through the migration experience before bring your own COBOL sources. It is not intended to showcase the full capabilities of the migration agents or the range of real-world transformations the project can perform.

### Converting the Sample Code to Java
We recommend adding the following instructions to the prompt in `JavaConverterAgent.cs`:
```
- Make sure to add a package declaration at the top of the Java files to match their folder path
- Make it runnable as a standalone Java application
- Convert COBOL `DISPLAY` statements to Java logging using a standard logging framework
- Assume that the Java code will be run as a scheduled job rather than as a web app or REST API
```

### Running the Java Code Locally
1. **Set up Maven**
   - Create `pom.xml`
   - Test and iterate with `mvn -q quarkus:run`
2. **Run in Docker**
   - Create `Dockerfile`
   - Build: `docker build -t hello-quarkus .`
   - Run: `docker run --rm hello-quarkus`

### Running the Java Code on Azure
1. **Provision Azure resources**: Azure Container Registry, Azure Container Apps, and Azure Log Analytics. Sample Bicep templates are available in the `infra/` directory.
2. **Build and push container image**: Build the container and push the image to Azure Container Registry (e.g., using `az acr build`)
3. **Deploy to Container Apps**: Configure Azure Container Apps to pull the image from the Azure Container Registry
4. **Monitor logs**: View Log Analytics to see "Hello, world!" being logged
