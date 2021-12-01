package p2p.chat.common

case class LocalTimestamp(timestamp: Long) extends Ordered[LocalTimestamp]:
  def compare(that: LocalTimestamp): Int = this.timestamp.compare(that.timestamp)

object LocalTimestamp:
  def now = LocalTimestamp(System.currentTimeMillis())
