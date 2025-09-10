targetScope = 'subscription'

@description('Name of the resource group to create')
param rgName string = 'rg-sample-java-output'

@description('Location for the resource group')
param location string = 'eastus'

resource rg 'Microsoft.Resources/resourceGroups@2021-04-01' = {
  name: rgName
  location: location
  properties: {}
}

output resourceGroupName string = rg.name
output resourceGroupLocation string = rg.location

// Parameters for container app and registry
@description('Name for the Container Registry')
param acrName string = 'samplejavaacr'

@description('SKU for the Container Registry')
param acrSku string = 'Basic'

@description('Name for the Container Apps managed environment')
param envName string = 'sample-containerenv'

@description('Container image for the job')
param jobImage string = 'mcr.microsoft.com/azuredocs/containerapps-helloworld:latest'

// Deploy resource-group scoped resources using a module
module rgModule 'rg.bicep' = {
  name: 'deployRgResources'
  scope: resourceGroup(rg.name)
  params: {
    location: location
    acrName: acrName
    acrSku: acrSku
    envName: envName
    jobName: 'sample-scheduled-job'
    jobImage: jobImage
    jobCron: '*/1 * * * *'
  }
}

output acrLoginServer string = rgModule.outputs.acrLoginServer
output containerEnvironmentId string = rgModule.outputs.containerEnvironmentId
output jobId string = rgModule.outputs.jobId
