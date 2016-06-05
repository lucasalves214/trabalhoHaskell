{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
pRoutes = [parseRoutes|
   / HomeR GET
   /cadastro-usuario CadastroUsR GET POST
   /cadastro-aluno CadastroAlR GET POST
   /cadastro-prof CadastroPrR GET POST
   /cadastro-curso CadastroCsR GET POST
   /cadastro-discip CadastroDsR GET POST
   /boletim-update BoletimUpdateR GET POST
   /boletim BoletimR GET
   /listar-alunos ListarR GET
|]