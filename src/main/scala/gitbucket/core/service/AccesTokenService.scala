package gitbucket.core.service

import gitbucket.core.model.Profile._
import profile.api._

import gitbucket.core.model.{Account, AccessToken}
import gitbucket.core.util.StringUtil
import slick.dbio.{FailureAction, SuccessAction}

import scala.annotation.tailrec
import scala.util.Random


trait AccessTokenService {

  def makeAccessTokenString: String = {
    val bytes = new Array[Byte](20)
    Random.nextBytes(bytes)
    bytes.map("%02x".format(_)).mkString
  }

  def tokenToHash(token: String): String = StringUtil.sha1(token)

  /**
   * @retuen (TokenId, Token)
   */
  def generateAccessToken(userName: String, note: String): DBIO[(Int, String)] = {

    def hash0(token: String = makeAccessTokenString): DBIO[(String, String)] = {
      val hash = tokenToHash(token)
      for {
        exists <- AccessTokens.filter(_.tokenHash === hash.bind).exists.result
        if !exists
      } yield
        hash -> token
    }

    @tailrec
    def generate0(): DBIO[(Int, String)] = {
      hash0() match {
        case SuccessAction((hash, token)) =>
          ((AccessTokens returning AccessTokens.map(_.accessTokenId)) += AccessToken(
            userName  = userName,
            note      = note,
            tokenHash = hash)) map (_ -> token)
        case FailureAction(_) => generate0()
      }
    }

    generate0()
  }

  def getAccountByAccessToken(token: String): DBIO[Option[Account]] =
    Accounts
      .join(AccessTokens)
      .filter{ case (ac, t) => (ac.userName === t.userName) && (t.tokenHash === tokenToHash(token).bind) && (ac.removed === false.bind) }
      .map{ case (ac, t) => ac }
      .result
      .headOption

  def getAccessTokens(userName: String): DBIO[Seq[AccessToken]] =
    AccessTokens.filter(_.userName === userName.bind).sortBy(_.accessTokenId.desc).result

  def deleteAccessToken(userName: String, accessTokenId: Int): DBIO[Int] =
    AccessTokens filter (t => t.userName === userName.bind && t.accessTokenId === accessTokenId) delete

}

object AccessTokenService extends AccessTokenService
