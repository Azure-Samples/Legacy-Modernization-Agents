# syntax=docker/dockerfile:1

# --- Build stage ---
FROM maven:3.9-eclipse-temurin-17 AS build
WORKDIR /workspace
COPY . .
RUN mvn -q -e -DskipTests clean package

# --- Runtime stage ---
FROM gcr.io/distroless/java17:nonroot
WORKDIR /app
COPY --from=build /workspace/target/quarkus-app /app/
ENTRYPOINT ["java","-jar","quarkus-run.jar"]
