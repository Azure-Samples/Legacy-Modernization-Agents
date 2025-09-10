package com.example.hello;

import io.quarkus.runtime.QuarkusApplication;
import io.quarkus.runtime.annotations.QuarkusMain;
import org.jboss.logging.Logger;

/**
 * Java conversion of the COBOL program 'HELLO'.
 * 
 * Original COBOL logic:
 * - DISPLAY "Hello, world!".
 * - STOP RUN.
 * 
 * Conversion notes:
 * - The COBOL DISPLAY statement is mapped to Java logging using JBoss Logging
 * (Quarkus default).
 * - No variables or data division present in the original COBOL.
 * - No error handling required as the logic is trivial.
 * - The program is structured as a Quarkus CLI application (QuarkusMain) for
 * scheduled/batch execution.
 * - The application terminates after execution, matching COBOL's STOP RUN.
 */
@QuarkusMain
public class HelloApplication implements QuarkusApplication {

    // Use Quarkus/JBoss Logger for output, as per Quarkus best practices
    private static final Logger LOG = Logger.getLogger(HelloApplication.class);

    /**
     * Entry point for the Quarkus CLI application.
     * 
     * @param args Command-line arguments (not used)
     * @return exit code (0 for success)
     */
    @Override
    public int run(String... args) {
        // Equivalent to COBOL's DISPLAY "Hello, world!".
        LOG.info("Hello, world!");

        // Equivalent to COBOL's STOP RUN (program termination).
        // Returning 0 signals successful completion.
        return 0;
    }

    /**
     * Main method for standalone execution.
     * This allows the application to be run as a standard Java application.
     */
    public static void main(String... args) {
        io.quarkus.runtime.Quarkus.run(HelloApplication.class, args);
        System.exit(0);
    }
}