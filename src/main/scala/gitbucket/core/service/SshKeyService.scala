package gitbucket.core.service

import gitbucket.core.model.SshKey
import gitbucket.core.model.Profile._
import profile.api._

trait SshKeyService {

  def addPublicKey(userName: String, title: String, publicKey: String)(implicit db: Database): Unit = run {
    SshKeys += SshKey(userName = userName, title = title, publicKey = publicKey)
  }

  def getPublicKeys(userName: String)(implicit db: Database): List[SshKey] = run {
    SshKeys.filter(_.userName === userName.bind).sortBy(_.sshKeyId).result
  }.toList

  def deletePublicKey(userName: String, sshKeyId: Int)(implicit db: Database): Unit = run {
    SshKeys filter (_.byPrimaryKey(userName, sshKeyId)) delete
  }

}
