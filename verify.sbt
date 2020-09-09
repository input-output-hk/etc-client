verifyDependencies in verify ++= Seq(
  "org.scala-lang" % "scala-library" sha1 "4861b00d921952f2dc9a024198bc72590fd7dc5e",
  "com.trueaccord.scalapb" % "scalapb-runtime" sha1 "efc8fc4d491cd988d75c86787187bd1556086043",
  "com.trueaccord.lenses" % "lenses" sha1 "d97d2958814bcfe2f19e1ed2f0f03fd9da5a3961",
  "com.lihaoyi" % "fastparse" sha1 "aaf2048f9c6223220eac28c9b6a442f27ba83c55",
  "com.lihaoyi" % "fastparse-utils" sha1 "92da792e8608653317ed6eb456f935fbfb2316bc",
  "com.lihaoyi" % "sourcecode" sha1 "ef9a771975cb0860f2b42778c5cf1f5d76818979",
  "com.google.protobuf" % "protobuf-java" sha1 "b32aba0cbe737a4ca953f71688725972e3ee927c",
  "com.typesafe.akka" % "akka-actor" sha1 "c4c545cc1809a995dff5d816876571c0204a0fe9",
  "com.typesafe" % "config" sha1 "d6ac0ce079f114adce620f2360c92a70b2cb36dc",
  "org.scala-lang.modules" % "scala-java8-compat" sha1 "1e6f1e745bf6d3c34d1e2ab150653306069aaf34",
  "com.typesafe.akka" % "akka-slf4j" sha1 "985acf0acbf0cf6b1222754e5818709c90024857",
  "org.slf4j" % "slf4j-api" sha1 "da76ca59f6a57ee3102f8f9bd9cee742973efa8a",
  "com.typesafe.akka" % "akka-testkit" sha1 "b2ce3dfed156bc6bd46dac040ae1a0c39900f852",
  "com.typesafe.akka" % "akka-http" sha1 "0cb16ab344d6dd8c6d8f4b6ef22294ddba51ce5a",
  "com.typesafe.akka" % "akka-http-core" sha1 "74a9e50a935a1f0d07bc801911e0edf2d1370929",
  "com.typesafe.akka" % "akka-parsing" sha1 "21b97bdd98e40eac7739c22b7047196942dc1413",
  "ch.megard" % "akka-http-cors" sha1 "dc90711314aa3c30bfda6676f6b936422222a80a",
  "org.json4s" % "json4s-native" sha1 "337d78817ea87103d873237360b56370f84721be",
  "org.json4s" % "json4s-core" sha1 "1283a83f5c76572d79d35f4a6c70f39555e9ab91",
  "org.json4s" % "json4s-ast" sha1 "2f9c210840f663776a4e5b49bdf77a95af568d75",
  "org.json4s" % "json4s-scalap" sha1 "1ea710bd90f69e959f9f677275d7397bc42732e7",
  "com.thoughtworks.paranamer" % "paranamer" sha1 "619eba74c19ccf1da8ebec97a2d7f8ba05773dd6",
  "org.scala-lang.modules" % "scala-xml" sha1 "e22de3366a698a9f744106fb6dda4335838cf6a7",
  "de.heikoseeberger" % "akka-http-json4s" sha1 "d1dc1d89f91617dc7a50977beed98a444e7f96f3",
  "com.typesafe.akka" % "akka-stream" sha1 "f38b3c3e496ddf6ff372dc94a375fbfe2e08e03f",
  "com.typesafe.akka" % "akka-protobuf" sha1 "652511d21b4b141c1e23380048cb5fb27d520e1f",
  "org.reactivestreams" % "reactive-streams" sha1 "323964c36556eb0e6209f65c1cef72b53b461ab8",
  "com.typesafe" % "ssl-config-core" sha1 "c1fdf6c0e7af6b5dd1df14e38062f917320413b4",
  "org.scala-lang.modules" % "scala-parser-combinators" sha1 "bbce493f8bf61b56623624ff96ac3865f7f6999a",
  "io.suzaku" % "boopickle" sha1 "a1cd66dfb24325d11cb38c340b7663e1ee4ed70e",
  "org.ethereum" % "rocksdbjni" sha1 "63e2b8b137c967ec15049f1f66b9d987e8f4beb7",
  "ch.qos.logback" % "logback-classic" sha1 "7c4f3c474fb2c041d8028740440937705ebb473a",
  "ch.qos.logback" % "logback-core" sha1 "864344400c3d4d92dfeb0a305dc87d953677c03c",
  "org.jline" % "jline" sha1 "dfb4e9e15e981634155ce063fa697b2b8964d507",
  "io.circe" % "circe-core" sha1 "f4f8674788f571d840ed98fabf3237f72c86d1f0",
  "io.circe" % "circe-numbers" sha1 "e8b931a2a2438d9ba84ff5ecbfb2a4ac7249b0d8",
  "org.scala-lang" % "scala-reflect" sha1 "23a60e62c5aebe21852ed40443e5b582dabc4d1a",
  "io.circe" % "circe-generic" sha1 "3cefb4701db62c625dd09ff580d8ea285f8d9c35",
  "org.typelevel" % "macro-compat" sha1 "ed809d26ef4237d7c079ae6cf7ebd0dfa7986adf",
  "com.chuusai" % "shapeless" sha1 "6041e2c4871650c556a9c6842e43c04ed462b11f",
  "io.circe" % "circe-parser" sha1 "9af9ad5e8a2027a7d93a2b21578f727f73f55d79",
  "io.circe" % "circe-jawn" sha1 "8462d202404f578f09cc9b89d7dca57dd94b09e5",
  "org.spire-math" % "jawn-parser" sha1 "e49f4a6294af0821d5348ad9f89a5ce8455fc1b3",
  "io.circe" % "circe-generic-extras" sha1 "712d9b815dc04ce45592b395882b0bf5dbe8bf66",
  "commons-io" % "commons-io" sha1 "815893df5f31da2ece4040fe0a12fd44b577afaf",
  "org.scala-sbt.ipcsocket" % "ipcsocket" sha1 "b671d32896b96c0311947309952078bf374a5c17",
  "net.java.dev.jna" % "jna" sha1 "65bd0cacc9c79a21c6ed8e9f588577cd3c2f85b9",
  "net.java.dev.jna" % "jna-platform" sha1 "00ab163522ed76eb01c8c9a750dedacb134fc8c0",
  "org.bouncycastle" % "bcprov-jdk15on" sha1 "2507204241ab450456bdb8e8c0a8f986e418bd99",
  "com.typesafe.scala-logging" % "scala-logging" sha1 "b6c6bb584f3e5c2d3f20aa7c8ff3e6959870b13c",
  "org.typelevel" % "mouse" sha1 "e88f9eeba20a6af2d11cdbde5edeb4110b5af8b2",
  "org.typelevel" % "cats-core" sha1 "92517f648a607008367c95b1ec6ea083d26bfc43",
  "org.typelevel" % "cats-macros" sha1 "217c7060d7348055bc11ce36cae8f6aa6aa694e0",
  "org.typelevel" % "machinist" sha1 "8b135f558a088ce4c0e036c762f5b6a5d4d5e4a8",
  "org.typelevel" % "cats-kernel" sha1 "8190477d30b17a963ff11922df854e108093aeb5",
  "com.twitter" % "util-collection" sha1 "8c62e0dc1a7bccd094ea3cfed23fe67ddd3a5590",
  "com.twitter" % "util-core" sha1 "ff2e5929ac0e26d1c8cfac00384707e8c6accb01",
  "com.twitter" % "util-function" sha1 "2b24ea268e08431eec442a510bf2a05f1d5a1a3b",
  "com.google.guava" % "guava" sha1 "54fed371b4b8a8cce1e94a9abd9620982d3aa54b",
  "com.google.guava" % "failureaccess" sha1 "1dcf1de382a0bf95a3d8b0849546c88bac1292c9",
  "com.google.guava" % "listenablefuture" sha1 "b421526c5f297295adef1c886e5246c39d4ac629",
  "com.google.code.findbugs" % "jsr305" sha1 "25ea2e8b0c338a877313bd4672d3fe056ea78f0d",
  "org.checkerframework" % "checker-qual" sha1 "eb2e8ab75598548cc8acf9a1ca227e480e01881e",
  "com.google.errorprone" % "error_prone_annotations" sha1 "d1a0c5032570e0f64be6b4d9c90cdeb103129029",
  "com.google.j2objc" % "j2objc-annotations" sha1 "ba035118bc8bac37d7eff77700720999acd9986d",
  "org.codehaus.mojo" % "animal-sniffer-annotations" sha1 "f97ce6decaea32b36101e37979f8b647f00681fb",

  "com.github.scopt" % "scopt" sha1 "e078455e1a65597146f8608dab3247bf1eb92e6e",
  "com.datadoghq" % "java-dogstatsd-client" sha1 "a9380127a42855a76af7787840a3a04b9fc4ce20",
  "org.xerial.snappy" % "snappy-java" sha1 "307b286efd119ad2c6d4291128bf110bddc68088"
)

verifyOptions in verify := VerifyOptions(
  includeBin = true,
  includeScala = true,
  includeDependency = true,
  excludedJars = Nil,
  warnOnUnverifiedFiles = false,
  warnOnUnusedVerifications = false
)

