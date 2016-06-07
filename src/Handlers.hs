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
            areq textField "Matrícula" Nothing <*>
            areq (selectField course) "Curso" Nothing

course = do
       entidades <- runDB $ selectList [] [Asc CursoSigla] 
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

formDisci :: Form Disciplinas
formDisci = renderDivs $ Disciplinas <$>
            areq (selectField crse) "Curso" Nothing <*>
            areq textField "Nome da Disciplina" Nothing <*>
            areq textField FieldSettings{fsId=Just "hident3",
                           fsLabel="Sigla",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","3")]} Nothing

crse = do
       entidades <- runDB $ selectList [] [Asc CursoSigla] 
       optionsPairs $ fmap (\ent -> (cursoSigla $ entityVal ent, entityKey ent)) entidades
        
formProf :: Form Professor
formProf = renderDivs $ Professor <$>
            areq textField "Nome" Nothing <*>
            areq textField "RG" Nothing <*>
            areq textField "Telefone" Nothing <*>
            areq textField "CEP" <*>
            areq textField "Logradouro" Nothing <*>
            areq textField "Numero" Nothing <*>
            areq textField "Bairro" Nothing <*>
            areq textField "Cidade" Nothing <*>
            areq textField "Estado" Nothing <*>
            areq textField "Matrícula" Nothing <*>
            areq (selectField crso) "Curso" Nothing

crso = do
       entidades <- runDB $ selectList [] [Asc CursoSigla] 
       optionsPairs $ fmap (\ent -> (cursoSigla $ entityVal ent, entityKey ent)) entidades
            
formUsuariop :: Form Usuariop
formUsuariop = renderDivs $ Usuariop <$>
             areq textField "Nome de usuário" Nothing <*>
             areq textField "Senha" Nothing <*>
             areq intField "ID de estudante" Nothing
             
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

getCadastroDsR :: Handler Html
getCadastroDsR = do
             (widget, enctype) <- generateFormPost formDisci
             defaultLayout $ do 
                 addStylesheet $ StaticR teste_css
                 widgetForm CadastroDsR enctype widget "Disciplinas"
                 
getCadastroUsPR :: Handler Html
getCadastroUsPR = do
             (widget, enctype) <- generateFormPost formUsuariop
             defaultLayout $ do 
                 addStylesheet $ StaticR teste_css
                 widgetForm CadastroUsPR enctype widget "Usuariop"
                 
getCadastroPrR :: Handler Html
getCadastroPrR = do
             (widget, enctype) <- generateFormPost formAluno
             defaultLayout $ do 
                 addStylesheet $ StaticR teste_css
                 widgetForm CadastroPrR enctype widget "Professor"

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
                <p> Curso #{cursoNome curso}
            |]

getBoletimR :: Handler Html
getBoletimR = defaultLayout [whamlet|Hello World!|]

getListarR :: Handler Html
getListarR = defaultLayout [whamlet|Hello World!|]

getBoletimUpdateR :: Handler Html
getBoletimUpdateR = defaultLayout [whamlet|Hello World!|]

postCadastroAlR :: Handler Html
postCadastroAlR = do
                ((result, _), _) <- runFormPost formAluno
                case result of
                    FormSuccess aluno -> do
                       runDB $ insert aluno
                       defaultLayout [whamlet|
                           <h1> Aluno #{alunoNome aluno} inserido com sucesso. 
                       |]
                    _ -> redirect CadastroAlR

postCadastroUsAR :: Handler Html
postCadastroUsAR = do
                ((result, _), _) <- runFormPost formUsuarioa
                case result of
                    FormSuccess usuarioa -> do
                       runDB $ insert usuarioa
                       defaultLayout [whamlet|
                           <h1> Nome de usuario #{usuarioaLogin usuarioa} criado com sucesso. 
                       |]
                    _ -> redirect CadastroUsAR
                    
postCadastroCsR :: Handler Html
postCadastroCsR = do
                ((result, _), _) <- runFormPost formCurso
                case result of
                    FormSuccess curso -> do
                       runDB $ insert curso
                       defaultLayout [whamlet| 
                           <h1> Curso #{cursoNome curso} criado com sucesso. 
                       |]
                    _ -> redirect CadastroCsR

postCadastroDsR :: Handler Html
postCadastroDsR = do
                ((result, _), _) <- runFormPost formDisci
                case result of
                    FormSuccess disci -> do
                       runDB $ insert disci
                       defaultLayout [whamlet| 
                           <h1> Disciplina #{disciplinasNome disci} inserida com sucesso. 
                       |]
                    _ -> redirect CadastroDsR

postCadastroPrR :: Handler Html
postCadastroPrR = do
                ((result, _), _) <- runFormPost formProf
                case result of
                    FormSuccess prof -> do
                       runDB $ insert prof
                       defaultLayout [whamlet| 
                           <h1> Professor #{professorNome prof} inserido com sucesso. 
                       |]
                    _ -> redirect CadastroCsR

postCadastroUsPR :: Handler Html
postCadastroUsPR = do
                ((result, _), _) <- runFormPost formUsuariop
                case result of
                    FormSuccess usuariop -> do
                       runDB $ insert usuariop
                       defaultLayout [whamlet|
                           <h1> Nome de usuario #{usuariopLogin usuariop} criado com sucesso. 
                       |]
                    _ -> redirect CadastroUsPR