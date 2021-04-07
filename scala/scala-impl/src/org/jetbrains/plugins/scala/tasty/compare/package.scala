package org.jetbrains.plugins.scala.tasty

import com.intellij.notification.{Notification, NotificationType, Notifications}
import com.intellij.util.AlarmFactory
import org.jetbrains.plugins.scala.NlsString

import scala.concurrent.duration.DurationInt

package object compare {

  def showNotification(msg: String): Unit = {
    val title = NlsString.force("Compare with TASTy")
    val content = NlsString.force(msg)
    val notification = new Notification(title, title, content, NotificationType.INFORMATION)
    Notifications.Bus.notify(notification)
    val expireRunnable: Runnable = () => notification.expire()
    AlarmFactory.getInstance.create.addRequest(expireRunnable, 5.seconds.toMillis)
  }
}
