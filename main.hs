{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Main where
import Import
import Yesod
import Yesod.Static
import Foundation
import Handlers
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql

connStr = "dbname=d8c2dh6d0guhns host=ec2-54-243-62-211.compute-1.amazonaws.com user=uasjrpwrzywotl password=OBtizj3NVdGwnA9s5TSu7J1fYD port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool 
       t@(Static settings) <- static "static"
       warp 8080 (Sitio t pool)
