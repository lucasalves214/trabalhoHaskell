{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
pRoutes = [parseRoutes|
   / HomeR GET
   /cadastro-usuario-aluno CadastroUsAR GET POST
   /cadastro-aluno CadastroAlR GET POST
   /cadastro-curso CadastroCsR GET POST
   /perfil-aluno/#AlunoId PerfilAlR GET
   /cadastro-usuario-prof CadastroUsPR GET
   /cadastro-prof CadastroPrR GET
   /cadastro-discip CadastroDsR GET
   /boletim-update BoletimUpdateR GET
   /boletim BoletimR GET
   /listar-alunos ListarR GET
   /static StaticR Static getStatic
|]