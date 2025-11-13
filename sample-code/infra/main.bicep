@minLength(3)
@description('A unique suffix for all resources')
param suffix string

@description('Location for the resources in this resource group')
param location string = resourceGroup().location

@description('SKU for the Container Registry')
param acrSku string = 'Basic'

@description('Container image for the scheduled job')
param jobImage string = 'mcr.microsoft.com/azuredocs/aci-helloworld:latest'

@description('Cron schedule for the job')
param jobCron string = '*/1 * * * *' // every minute

// Generate unique resource names based on suffix and Azure best practice abbreviations
var acrName = toLower('acr${suffix}') // Azure Container Registry (does not allow hyphens)
var caeName = toLower('cae-${suffix}') // Container Apps Environment
var cajName = toLower('caj-${suffix}') // Container Apps Job
var logAnalyticsName = toLower('log-${suffix}') // Log Analytics Workspace

// Log Analytics workspace for monitoring
resource logAnalytics 'Microsoft.OperationalInsights/workspaces@2025-02-01' = {
  name: logAnalyticsName
  location: location
  properties: {
    sku: {
      name: 'PerGB2018'
    }
    retentionInDays: 30
  }
}

resource acr 'Microsoft.ContainerRegistry/registries@2025-04-01' = {
  name: acrName
  location: location
  sku: {
    name: acrSku
  }
  properties: {
    adminUserEnabled: false
  }
}

resource containerEnv 'Microsoft.App/managedEnvironments@2025-01-01' = {
  name: caeName
  location: location
  properties: {
    appLogsConfiguration: {
      destination: 'log-analytics'
      logAnalyticsConfiguration: {
        customerId: logAnalytics.properties.customerId
        sharedKey: logAnalytics.listKeys().primarySharedKey
      }
    }
  }
}

resource job 'Microsoft.App/jobs@2025-01-01' = {
  name: cajName
  location: location
  properties: {
    environmentId: containerEnv.id
    configuration: {
      triggerType: 'Schedule'
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
        }
      ]
    }
  }
}


output logAnalyticsWorkspaceId string = logAnalytics.id
output logAnalyticsWorkspaceName string = logAnalytics.name

output acrLoginServer string = acr.properties.loginServer
output containerEnvironmentId string = containerEnv.id
