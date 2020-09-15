package io.iohk.ethereum.utils

import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.utils.NumericUtils._

import scala.collection.JavaConverters._

import com.typesafe.config.{Config => TypesafeConfig}

import scala.util.Try

case class BlockchainConfig(
  frontierBlockNumber: BigInt,
  homesteadBlockNumber: BigInt,
  eip106BlockNumber: BigInt,
  eip150BlockNumber: BigInt,
  eip155BlockNumber: BigInt,
  eip160BlockNumber: BigInt,
  eip161BlockNumber: BigInt,
  byzantiumBlockNumber: BigInt,
  constantinopleBlockNumber: BigInt,
  istanbulBlockNumber: BigInt,

  atlantisBlockNumber: BigInt,
  aghartaBlockNumber: BigInt,
  phoenixBlockNumber: BigInt,
  petersburgBlockNumber: BigInt,

  maxCodeSize: Option[BigInt],
  difficultyBombPauseBlockNumber: BigInt,
  difficultyBombContinueBlockNumber: BigInt,
  difficultyBombRemovalBlockNumber: BigInt,

  customGenesisFileOpt: Option[String],

  daoForkConfig: Option[DaoForkConfig],

  accountStartNonce: UInt256,

  chainId: Byte,
  networkId: Int,

  monetaryPolicyConfig: MonetaryPolicyConfig,

  gasTieBreaker: Boolean,

  ethCompatibleStorage: Boolean,

  bootstrapNodes: Set[String]
)

object BlockchainConfig {

  // scalastyle:off method.length
  def fromRawConfig(blockchainConfig: TypesafeConfig): BlockchainConfig = {
    val frontierBlockNumber: BigInt = BigInt(blockchainConfig.getString("frontier-block-number"))
    val homesteadBlockNumber: BigInt = BigInt(blockchainConfig.getString("homestead-block-number"))
    val eip106BlockNumber: BigInt = BigInt(blockchainConfig.getString("eip106-block-number"))
    val eip150BlockNumber: BigInt = BigInt(blockchainConfig.getString("eip150-block-number"))
    val eip155BlockNumber: BigInt = BigInt(blockchainConfig.getString("eip155-block-number"))
    val eip160BlockNumber: BigInt = BigInt(blockchainConfig.getString("eip160-block-number"))
    val eip161BlockNumber: BigInt = BigInt(blockchainConfig.getString("eip161-block-number"))
    val byzantiumBlockNumber: BigInt = BigInt(blockchainConfig.getString("byzantium-block-number"))
    val constantinopleBlockNumber: BigInt = BigInt(blockchainConfig.getString("constantinople-block-number"))
    val istanbulBlockNumber: BigInt = BigInt(blockchainConfig.getString("istanbul-block-number"))

    val atlantisBlockNumber: BigInt = BigInt(blockchainConfig.getString("atlantis-block-number"))
    val aghartaBlockNumber: BigInt = BigInt(blockchainConfig.getString("agharta-block-number"))
    val phoenixBlockNumber: BigInt = BigInt(blockchainConfig.getString("phoenix-block-number"))
    val petersburgBlockNumber: BigInt = BigInt(blockchainConfig.getString("petersburg-block-number"))

    val maxCodeSize: Option[BigInt] = Try(BigInt(blockchainConfig.getString("max-code-size"))).toOption
    val difficultyBombPauseBlockNumber: BigInt = BigInt(blockchainConfig.getString("difficulty-bomb-pause-block-number"))
    val difficultyBombContinueBlockNumber: BigInt = BigInt(blockchainConfig.getString("difficulty-bomb-continue-block-number"))
    val difficultyBombRemovalBlockNumber: BigInt = BigInt(blockchainConfig.getString("difficulty-bomb-removal-block-number"))
    val customGenesisFileOpt: Option[String] = Try(blockchainConfig.getString("custom-genesis-file")).toOption

    val daoForkConfig = Try(blockchainConfig.getConfig("dao")).toOption.map(DaoForkConfig(_))
    val accountStartNonce: UInt256 = UInt256(BigInt(blockchainConfig.getString("account-start-nonce")))

    val chainId: Byte = {
      val s = blockchainConfig.getString("chain-id")
      val n = parseHexOrDecNumber(s)
      require(n >= 0 && n <= 127, "chain-id must be a number in range [0, 127]")
      n.toByte
    }

    val networkId: Int = blockchainConfig.getInt("network-id")

    val monetaryPolicyConfig = MonetaryPolicyConfig(blockchainConfig.getConfig("monetary-policy"))

    val gasTieBreaker: Boolean = blockchainConfig.getBoolean("gas-tie-breaker")

    val ethCompatibleStorage: Boolean = blockchainConfig.getBoolean("eth-compatible-storage")

    val bootstrapNodes: Set[String] = blockchainConfig.getStringList("bootstrap-nodes").asScala.toSet

    BlockchainConfig(
      frontierBlockNumber = frontierBlockNumber,
      homesteadBlockNumber = homesteadBlockNumber,
      eip106BlockNumber = eip106BlockNumber,
      eip150BlockNumber = eip150BlockNumber,
      eip155BlockNumber = eip155BlockNumber,
      eip160BlockNumber = eip160BlockNumber,
      eip161BlockNumber = eip161BlockNumber,
      byzantiumBlockNumber = byzantiumBlockNumber,
      constantinopleBlockNumber = constantinopleBlockNumber,
      istanbulBlockNumber = istanbulBlockNumber,

      atlantisBlockNumber = atlantisBlockNumber,
      aghartaBlockNumber = aghartaBlockNumber,
      phoenixBlockNumber = phoenixBlockNumber,
      petersburgBlockNumber = petersburgBlockNumber,

      maxCodeSize = maxCodeSize,
      difficultyBombPauseBlockNumber = difficultyBombPauseBlockNumber,
      difficultyBombContinueBlockNumber = difficultyBombContinueBlockNumber,
      difficultyBombRemovalBlockNumber = difficultyBombRemovalBlockNumber,
      customGenesisFileOpt = customGenesisFileOpt,

      daoForkConfig = daoForkConfig,
      accountStartNonce = accountStartNonce,

      chainId = chainId,

      networkId = networkId,

      monetaryPolicyConfig = monetaryPolicyConfig,

      gasTieBreaker = gasTieBreaker,

      ethCompatibleStorage = ethCompatibleStorage,

      bootstrapNodes = bootstrapNodes
    )
  }
  // scalastyle:on method.length
}
