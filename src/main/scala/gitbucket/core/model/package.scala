package gitbucket.core

package object model {
  type Session = slick.jdbc.JdbcBackend#Session
  type Database = slick.jdbc.JdbcBackend#Database
}
