# Release Management

A Continuous Deployment (CD) pipeline will be used in order to ease the releasing and deploying of the tutor. For this, two workflows have been created: one that builds artifacts, and one that builds a docker image. Before the build artifacts job runs, tests will be executed in order to check if the system is still stable. 

```plantuml
@startuml

actor " " as dev

storage "Github Repository" as git

node "Build Artifacts" as cd
node "Build Docker Image" as cd2

cloud "Docker Hub" as hub

artifact Binary as bin
artifact "Example backend configuration" as back
artifact Frontend as front


dev -> git : Tags release
git -> cd : Triggers workflow to run
git -> cd2 : Triggers workflow to run

cd -d[hidden]- cd2

cd -d-> bin : Attach artifact\nto release
cd -d-> back : Attach artifact\nto release
cd -d-> front : Attach artifact\nto release

bin -d[hidden]- back
back -d[hidden]- front

cd2 -d-> hub : Upload artifact

@enduml
```

![Deploying a new release](images/release-flow.png)

## Considerations for running in the cloud

When deploying the tutor using cloud infrastructure, the docker image that is build can be used. Multiple online cloud service providers offer the running of docker containers:

- Amazon Web Services offers Amazon Elastic Compute Cloud
- Google Cloud Platform offers Google Kubernetes Engine
- Oracle Cloud Infrastructure offers Container Instances

