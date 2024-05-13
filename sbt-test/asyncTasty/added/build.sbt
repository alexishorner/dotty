// scalacOptions ++= Seq("-Yasync-tasty")
// scalacOptions ++= Seq("-Yasync-tasty", "-Ydump-sbt-inc")
scalacOptions ++= Seq("-Yasync-tasty", "-Ydisable-extract-api", "-Ycompare-async-tasty")

scriptedBufferLog := false