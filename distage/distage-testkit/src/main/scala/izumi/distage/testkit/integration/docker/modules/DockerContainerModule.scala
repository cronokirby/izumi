package izumi.distage.testkit.integration.docker.modules

import com.github.dockerjava.api.command.DockerCmdExecFactory
import distage.TagK
import izumi.distage.config.annotations.ConfPath
import izumi.distage.model.definition.ModuleDef
import izumi.distage.model.monadic.DIEffect
import izumi.distage.testkit.integration.docker.{Docker, DockerClientWrapper, DockerCmdExecFactoryResource}

class DockerContainerModule[F[_]: DIEffect: TagK] extends ModuleDef {
  // expose DockerClientWrapper.Resource to execute its IntegrationCheck https://github.com/7mind/izumi/issues/563
  make[DockerClientWrapper.Resource[F]]
  make[DockerClientWrapper[F]].refResource[DockerClientWrapper.Resource[F]]
  make[DockerCmdExecFactory].fromResource[DockerCmdExecFactoryResource[F]]

  include(DockerContainerModule.config)
}

object DockerContainerModule {
  final val config = new ModuleDef {
    make[Docker.ClientConfig].from {
      conf: Docker.ClientConfig @ConfPath("docker") =>
        conf
    }
  }
}
