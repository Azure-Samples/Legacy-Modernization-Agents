@description('Location for the resources in this resource group')
param location string

@description('Name for the Container Registry')
param acrName string

@description('SKU for the Container Registry')
param acrSku string = 'Basic'

@description('Name for the Container Apps managed environment')
param envName string

@description('Container image to deploy to the Container App (kept for compatibility)')
param containerAppName string = ''
param containerImage string = ''

@description('Name for the scheduled job')
param jobName string

@description('Container image for the scheduled job')
param jobImage string

@description('Cron schedule for the job')
param jobCron string = '*/1 * * * *' // every minute

resource acr 'Microsoft.ContainerRegistry/registries@2023-01-01-preview' = {
  name: acrName
  location: location
  sku: {
    name: acrSku
  }
  properties: {
    adminUserEnabled: false
  }
}

resource containerEnv 'Microsoft.App/managedEnvironments@2024-03-01' = {
  name: envName
  location: location
  properties: {
    // Default environment configuration; configure logging/tracing separately if needed
  }
}

// Optional: Deploy a scheduled Container Apps Job triggered by cron (every minute by default)
// Uses the same managed environment. If jobName is empty, the job resource won't be created.
resource job 'Microsoft.App/jobs@2024-03-01' = if (!empty(jobName)) {
  name: jobName
  location: location
  properties: {
    environmentId: containerEnv.id
    configuration: {
      // Trigger type and schedule configuration for jobs
      triggerType: 'Schedule'
      // Schedule trigger configuration for jobs: use cronExpression and numeric timeouts
      scheduleTriggerConfig: {
        cronExpression: jobCron
        parallelism: 1
        replicaCompletionCount: 1
      }
      replicaTimeout: 300
      replicaRetryLimit: 0
    }
    template: {
      containers: [
        {
          name: 'job'
          image: jobImage
          command: ['/bin/sh', '-c']
          args: ['echo "Scheduled job run"; sleep 1; exit 0']
        }
      ]
    }
  }
}

output jobId string = job.id

output acrLoginServer string = acr.properties.loginServer
output containerEnvironmentId string = containerEnv.id
