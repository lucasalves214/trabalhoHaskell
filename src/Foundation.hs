{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Import
import Yesod
import Yesod.Static
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Sitio = Sitio {getStatic :: Static, connPool :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Aluno
   nome Text
   rg Text
   telefone Text
   cep Text
   lograd Text
   numr Text
   bairro Text
   cidade Text
   uf Text sqltype=varchar(2)
   matricula Text
   cursoid CursoId
   deriving Show
   
Usuarioa
   login Text
   senha Text
   aluno AlunoId
   deriving Show
   
Curso
   nome Text
   sigla Text sqltype=varchar(3)
   deriving Show

Disciplinas
   nome Text
   sigla Text sqltyoe=varchar(3)
   curso CursoId
   deriving Show
   
Professor
   nome Text
   rg Text
   telefone Text
   cep Text
   lograd Text
   numr Text
   bairro Text
   cidade Text
   uf Text
   matricula Text
   curso CursoId
   deriving Show
   
Usuariop
   login Text
   senha Text
   professor ProfessorId
   deriving Show
|]

staticFiles "static"

mkYesodData "Sitio" pRoutes

mkMessage "Sitio" "language" "pt-br"

instance Yesod Sitio where
    authRoute _ = Just LoginR
    
    isAuthorized HomeR _ = return Authorized
    isAuthorized CadastroUsAR _ = return Authorized
    isAuthorized CadastroUsPR _ = return Authorized
    isAuthorized AdminR _ = isAdmin
    isAuthorized CadastroAlR _ = isAdmin
    isAuthorized CadastroPrR _ = isAdmin
    isAuthorized CadastroCsR _ = isAdmin
    isAuthorized CadastroDsR _ = isAdmin
    isAuthorized BoletimUpdateR _ = isUserp
    isAuthorized _ _ = isUsera

isUsera = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized
        
isUserp = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized
    
isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized 
        Just _ -> Unauthorized "Área restrita"
        
instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod Sitio where

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage