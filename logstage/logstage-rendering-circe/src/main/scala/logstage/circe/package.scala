package logstage

import izumi.logstage.api.rendering.json

package object circe extends LogstageCirce {

  override type LogstageCirceRenderingPolicy = json.LogstageCirceRenderingPolicy

}
