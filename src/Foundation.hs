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
   curso CursoId
   telefone Text
   cep Text
   lograd Text
   numr Text
   bairro Text
   cidade Text
   uf Text
   deriving Show
   
Usuario
   login Text
   senha Text
   aluno AlunoId
   professor ProfessorId
   deriving Show
   
Curso
   nome Text
   sigla Text sqltype=varchar(3)
   deriving Show

Discisciplinas
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
   curso CursoId
   disciplina DisciplinasId
   deriving Show
|]

staticFiles "static"

mkYesodData "Sitio" pRoutes

mkMessage "Sitio" "language" "pt-br"

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