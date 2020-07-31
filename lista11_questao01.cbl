       $set sourceformat"free"



       *>------------ divisão de identificação do programa

       identification division.
       *>Nome do programa
       program-id. "lista11_numero01.versaoarquivos".
       *>Nome do autor
       author. "Madona Schvambach".
       installation. "PC".
       *>Data que o programa foi escrito
       date-written.   24/07/2020.
       date-compiled.  24/07/2020.


       *>divisao para configuracao do programa
       environment division.
       configuration section.


       *>declarado que será utilizado vírgulo ao invés de ponto
       special-names. decimal-point is comma.


       *>declaracao de recursos externos
       input-output Section.

       file-control.

       *>   --------- declaracao com nome do arquivo, tipo, modo de acesso e status
       *>   nome lógico e e arquivo de memoria
       select arqTemperaturas assign to "arqTemperaturas.txt"
       *>   tipo de arquivo (sequencial)
       organization    is line sequential
       *>   modo de acesso ao arquivo (sequencial)
       access mode     is sequential
       *>   evita perda de dados em ambientes multi-usuarios(varios usuarios entrando com dados ao mesmo tempo)
       lock mode is automatic
       *>   variavel "ws-fs-arqAlunos" retona o status do arquivo (0, 35....)
       file status     is  ws-fs-arqTemperaturas.

       i-o-control.


       *>  ------- declaracao de variaveis
       data division.



       *> ------- variáveis de arquivos
       file section.


       fd arqTemperaturas.

       01  fd-temperaturas.
           05  fd-temperatura                      pic z9,99.


       *> ------- variavéis de trabalho
       working-storage section.


       77  ws-fs-arqTemperaturas                   pic 9(02).


       01  ws-temperaturas occurs 30.
           05  ws-temperatura                      pic 9(02)v9(02).


       01 ws-msn-erro.
           05 ws-msn-erro-ofsset                   pic 9(04).
           05 filler                               pic x(01) value "-".
           05 ws-msn-erro-cod                      pic 9(02).
           05 filler                               pic x(01) value space.
           05 ws-msn-erro-text                     pic x(42).


       77  ws-ind-temp                             pic 9(02).


       77  ws-i                                    pic 9(02).
       77  ws-soma                                 pic 9(03)v9(02) value 0.
       77  ws-media                                pic z9,99.
       77  ws-dia                                  pic 9(02).
       77  ws-opcao                                pic x(01).


       *>------Variaveis para comunicaçao entre programa
       linkage section.



       *>------Declaração de tela
       screen section.


       *>DECLARAÇÃO DO CORPO DO PROGRAMA
       procedure division.

           perform inicializacao.
           perform processamento.
           perform finalizacao.


       *>------------------------------------------------------------------------
       *>   inicialização do programa
       *>------------------------------------------------------------------------
       inicializacao section.


           perform leitura-inicial-arquivo

           *>somar todas as temps do arquivo
           perform somar-temperaturas

           display "---- TEMPERATURA ----"
           display " "


                   .
       inicializacao-exit.
                   exit.



       *>------------------------------------------------------------------------
       *>   processamento do programa
       *>------------------------------------------------------------------------
       processamento section.

           perform until ws-opcao = "N" or ws-opcao = "n"

               display "Informe o dia:"
               accept ws-dia

               if ws-dia < 01 or ws-dia > 30 then
                   display erase
                   display "-- Dia invalido! --"
                   display " "
               else
                   display erase
                   display "Dia: "         ws-dia
                   display " "
                   display "Temperatura: " ws-temperatura(ws-dia)
                   display " "
                   display "media eh: "    ws-media

                   perform conferir-temp-dia-media

                   display " "
                   display "Deseja continuar?"
                   display "   'S'im ou 'N'ao"
                   accept ws-opcao

                   display erase

               end-if

           end-perform


           .
       processamento-exit.
           exit.


       *>------------------------------------------------------------------------
       *>   finalizacao do programa
       *>------------------------------------------------------------------------
       finalizacao section.


           display "--- fim ---"
           Stop run


           .
       finalizacao-exit.
           exit.



       *>------------------------------------------------------------------------
       *>   leitura inicial do arquivo
       *>------------------------------------------------------------------------
       leitura-inicial-arquivo section.


           open input arqTemperaturas

           *>conferir se possui erro ao abrir arquivo
           if ws-fs-arqTemperaturas <> 0
               and ws-fs-arqTemperaturas <> 5 then
               move 1                                     to ws-msn-erro-ofsset
               move ws-fs-arqTemperaturas                 to ws-msn-erro-cod
               display "File Status ao abrir arquivo(input): "  ws-fs-arqTemperaturas
               perform finaliza-anormal

           end-if


           .
       leitura-inicial-arquivo-exit.
           exit.

       *>------------------------------------------------------------------------
       *>   somar temperaturas
       *>------------------------------------------------------------------------
       somar-temperaturas section.


           move 0 to ws-soma

           perform varying ws-ind-temp from 1 by 1 until ws-fs-arqTemperaturas = 10
                                                       or ws-ind-temp > 30

               *> vai ler e mover todas as variaveis do arqTemperaturas para ws-temperaturas(ws-ind-temp)
               read arqTemperaturas into ws-temperatura(ws-ind-temp)

               if  ws-fs-arqTemperaturas <> 0 and ws-fs-arqTemperaturas <> 10 then

                   move 2                                to ws-msn-erro-ofsset
                   move ws-fs-arqTemperaturas            to ws-msn-erro-cod
                   move "Erro ao ler arq. arqEstados "   to ws-msn-erro-text
                   perform finaliza-anormal

               else
                   compute ws-soma = ws-soma + ws-temperatura(ws-ind-temp)

               end-if

           end-perform

           compute ws-media = ws-soma/30


           .
       somar-temperaturas-exit.
           exit.


       *>------------------------------------------------------------------------
       *>   conferir se dia de entrada é maior que a media
       *>------------------------------------------------------------------------
       conferir-temp-dia-media section.


           display " "
           if ws-temperatura(ws-dia) > ws-media then
               display "Temperatura acima da media"
           else
               display "Temperatura abaixo da media"
           end-if


           .
       conferir-temp-dia-media-exit.
           exit.


       *>------------------------------------------------------------------------
       *>   finaliza anormal
       *>------------------------------------------------------------------------
       finaliza-anormal section.


           close arqTemperaturas
           if ws-fs-arqTemperaturas <> 0
               and ws-fs-arqTemperaturas <> 5 then
               move 1                                              to ws-msn-erro-ofsset
               move ws-fs-arqTemperaturas                          to ws-msn-erro-cod
               display "File Status ao fechar arquivo(input): "    ws-fs-arqTemperaturas
               perform finaliza-anormal

           end-if

           display erase
           display ws-msn-erro.
           Stop run

           .
       finaliza-anormal-exit.
           exit.
