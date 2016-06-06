{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers where
import Import
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text

import Database.Persist.Postgresql

mkYesodDispatch "Sitio" pRoutes

formAluno :: Form Aluno
formAluno = renderDivs $ Aluno <$>
            areq textField "Nome" Nothing <*>
            areq textField "RG" Nothing <*>
            areq textField "Telefone" Nothing <*>
            areq textField "CEP" <*>
            areq textField "Logradouro" Nothing <*>
            areq textField "Numero" Nothing <*>
            areq textField "Bairro" Nothing <*>
            areq textField "Cidade" Nothing <*>
            areq textField "Estado" Nothing <*>
            areq (selectField crso) "Curso" Nothing

crso = do
       entidades <- runDB $ selectList [] [Asc CursoNome] 
       optionsPairs $ fmap (\ent -> (cursoSigla $ entityVal ent, entityKey ent)) entidades
       
formUsuarioa :: Form Usuarioa
formUsuarioa = renderDivs $ Usuarioa <$>
             areq textField "Nome de usuário" Nothing <*>
             areq textField "Senha" Nothing <*>
             areq intField "ID de estudante" Nothing

formCurso :: Form Curso
formCurso = renderDivs $ Curso <$>
            areq textField "Nome do Curso" Nothing <*>
            areq textField FieldSettings{fsId=Just "hident2",
                           fsLabel="Sigla",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","3")]} Nothing
       
widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Widget
widgetForm x enctype widget y = $(whamletFile "templates/form.hamlet")

getHomeR :: Handler Html
getHomeR =  defaultLayout [whamlet|Hello World!|]

getCadastroUsAR :: Handler Html
getCadastroUsAR = do
             (widget, enctype) <- generateFormPost formUsuarioa
             defaultLayout $ do 
                 addStylesheet $ StaticR teste_css
                 widgetForm CadastroUsAR enctype widget "Usuarioa"
                 
getCadastroAlR :: Handler Html
getCadastroAlR = do
             (widget, enctype) <- generateFormPost formAluno
             defaultLayout $ do 
                 addStylesheet $ StaticR teste_css
                 widgetForm CadastroAlR enctype widget "Aluno"

getCadastroCsR :: Handler Html
getCadastroCsR = do
             (widget, enctype) <- generateFormPost formCurso
             defaultLayout $ do 
                 addStylesheet $ StaticR teste_css
                 widgetForm CadastroCsR enctype widget "Curso"
                    
getPerfilAlR :: AlunoId -> Handler Html
getPerfilAlR pid = do
             aluno <- runDB $ get404 pid 
             curso <- runDB $ get404 (alunoCursoid aluno)
             defaultLayout [whamlet|
                <h1> Seja bem-vindo #{alunoNome aluno}
                <p> RG: #{alunoRg aluno}
                <p> Telefone: #{alunoTelefone aluno}
                <p> CEP: #{alunoCep aluno}
                <p> Logradouro: #{alunoLograd aluno}
                <p> N: #{alunoNumr aluno}
                <p> Bairro: #{alunoBairro aluno}
                <p> Cidade: #{alunoCidade aluno}
                <p> Estado: #{alunoUf aluno}
                <h1> Curso #{cursoNome curso}
            |]

getCadastroUsPR :: Handler Html
getCadastroUsPR = defaultLayout [whamlet|Hello World!|]

getCadastroPrR :: Handler Html
getCadastroPrR = defaultLayout [whamlet|Hello World!|]

getCadastroDsR :: Handler Html
getCadastroDsR = defaultLayout [whamlet|Hello World!|]

getBoletimUpdateR :: Handler Html
getBoletimUpdateR = defaultLayout [whamlet|Hello World!|]

getBoletimR :: Handler Html
getBoletimR = defaultLayout [whamlet|Hello World!|]

getListarR :: Handler Html
getListarR = defaultLayout [whamlet|Hello World!|]

postCadastroAlR :: Handler Html
postCadastroAlR = do
                ((result, _), _) <- runFormPost formAluno
                case result of
                    FormSuccess aluno -> do
                       runDB $ insert aluno
                       defaultLayout [whamlet|
                           <h1> #{alunoNome aluno} Inserido com sucesso. 
                       |]
                    _ -> redirect CadastroAlR

postCadastroUsAR :: Handler Html
postCadastroUsAR = do
                ((result, _), _) <- runFormPost formUsuarioa
                case result of
                    FormSuccess usuarioa -> do
                       runDB $ insert usuarioa
                       defaultLayout [whamlet|
                           <h1> #{usuarioaLogin usuarioa} Inserido com sucesso. 
                       |]
                    _ -> redirect CadastroUsAR
                    
postCadastroCsR :: Handler Html
postCadastroCsR = do
                ((result, _), _) <- runFormPost formCurso
                case result of
                    FormSuccess curso -> do
                       runDB $ insert curso
                       defaultLayout [whamlet| 
                           <h1> #{cursoNome curso} Inserido com sucesso. 
                       |]
                    _ -> redirect CadastroCsR