package io.iohk.ethereum

import io.iohk.ethereum.nodebuilder.{StdNode, TestNode}
import io.iohk.ethereum.utils.{Config, Logger}

import java.nio.file.{Files, Paths}
import java.util.logging.LogManager

object Mantis extends Logger {
  def main(args: Array[String]): Unit = {
    LogManager.getLogManager().reset(); // disable java.util.logging, ie. in legacy parts of jupnp

    val node =
      if (Config.testmode) {
        log.info("Starting Mantis in test mode")
        deleteRocksDBFiles()
        new TestNode
      } else new StdNode

    log.info("Mantis app {}", Config.clientVersion)
    log.info("Using network {}", Config.blockchains.network)

    node.start()
  }

  private def deleteRocksDBFiles(): Unit = {
    val path = Paths.get(Config.Db.RocksDb.path)
    if (path.toFile.exists()) {
      log.info("Deleting previous database {}", Config.Db.RocksDb.path)
      Files
        .list(path)
        .map(_.toFile)
        .filter(_.isFile)
        .forEach(f => f.delete())
    }
  }
}
