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
            areq textField "RG:" Nothing <*>
            areq textField "Telefone:" Nothing <*>
            areq textField "CEP:" <*>
            areq textField "Logradouro" Nothing <*>
            areq textField "Número:" Nothing <*>
            areq textField "Bairro:" Nothing <*>
            areq textField "Cidade:" <*>
            req textField FieldSettings{fsId=Just "hident9",
                           fsLabel="Estado",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","2")]} Nothing <*>
            areq (selectField curso) "Curso" Nothing
            
curso = do
       entidades <- runDB $ selectList [] [Asc CursoNome] 
       optionsPairs $ fmap (\ent -> (cursoSigla $ entityVal ent, entityKey ent)) entidades
       
formUsuarioA :: Form UsuarioA
formUsuarioA = renderDivs $ UsuarioA <$>
             areq intField "ID de estudante" Nothing <*>
             areq textField "Nome de usuário" Nothing <*>
             areq textField "Senha" Nothing

formCurso :: Form Curso
formCurso - renderDivs $ Curso <$>
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
             (widget, enctype) <- generateFormPost formUusarioA
             defaultLayout $ do 
                 addStylesheet $ StaticR teste_css
                 widgetForm CadastroUsAR enctype widget "UsuarioA"
                 
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
                 <p> Dados Pessoais
                 <p> RG: #{alunoRg aluno}
                 <p> Telefone: #{alunoTelefone aluno}
                 <p> CEP: #{alunoCep aluno}
                 <p> Logradouro: #{alunoLograd aluno}
                 <p> Nº: #{alunoNmr aluno}
                 <p> Bairro: #{alunoBairro aluno}
                 <p> Cidade: #{alunoCidade aluno}
                 <p> Estado: #{alunoUf aluno}
                 <h1> Curso #{cursoNome curso}
             |]

getCadastroUsPR :: Handler Html
getCadastroUsPR = defaultLayout [whamlet|Hello World!|]

getCadastroPrR :: Handler Html
getCadastroPrR = defaltLayout [whamlet|Hello World!|]

getCadastroDsR :: Handler Html
getCadastroDsR = defaultLayout [whamlet|Hello World!|]

getBoletimUpdateR :: Handler Html
getBoletimUpdateR = defaultLayout [whamlet|Hello World!|]

getBoletimBoletimR :: Handler Html
getBoletimBoletimR = defaultLayout [whamlet|Hello World!|]

getBoletimListarR :: Handler Html
getBoletimListarR = defaultLayout [whamlet|Hello World!|]

postCadastroUsAR :: Handler Html
postCadastroUsAR = do
                ((result, _), _) <- runFormPost formUsuarioA
                case result of
                    FormSuccess aluno -> do
                       runDB $ insert usuario
                       defaultLayout [whamlet| 
                           <h1> #{usuarioNome usuario} Inserido com sucesso. 
                       |]
                    _ -> redirect CadastroUsAR

postCadastroAlR :: Handler Html
postCadastroAlR = do
                ((result, _), _) <- runFormPost formUsuarioA
                case result of
                    FormSuccess aluno -> do
                       runDB $ insert aluno
                       defaultLayout [whamlet| 
                           <h1> #{alunoNome aluno} Inserido com sucesso. 
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