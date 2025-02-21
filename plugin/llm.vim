" Title:        LLM CLI Plugin
" Description:  A Plugin for integrating LLM CLI capabilities in vim.
" Last Change:  Feb 02 2025
" Maintainer:   Joe H <https://github.com/blockjoe>

" Prevents the plugin from being loaded multiple times. If the loaded
" variable exists, do nothing more. Otherwise, assign the loaded
" variable and continue running this instance of the plugin.

if exists("g:loaded_llm")
    finish
endif

let g:template_prefix = "gen"

function! OpenChatInBuffer(chat)
    execute "vsp Chat-" . a:chat
    if &modifiable
       setlocal buftype=nofile
       setlocal filetype=markdown
       execute 'r ! llm logs --cid ' . a:chat
       setlocal noma
    endif
endfunction

function! OpenLastChatInBuffer()
    let db_path = system('llm logs path')
    let chat = system('sqlite3 '  . '"' .  trim(l:db_path) . '"' . ' "select id from conversations order by id desc limit 1"')
    call OpenChatInBuffer(chat)
endfunction

function! RunCommandAndPaste(template)

  function! s:exitCb(job, exitStatus)
	let l:bufNr = ch_getbufnr(job_getchannel(a:job), 'out')
	call setbufvar(l:bufNr, '&buftype', 'nofile')
	call setbufvar(l:bufNr, '&modifiable', 1)

	sleep 200ms

	let choice = confirm('Do you wish to accept these changes?', "&Yes\n&No", 1)

	let content = getline(2, '$')[1:-2]

	if choice == 1
	   " Replace the selected text with the result
	    let @@ = content->join("\n")
	    execute 'b#'
	    normal! G$p

	    " Restore the selection and register settings
	    let &selection = @s
	    let @@ = @r
	    execute 'b#'
	endif
	execute 'bw'
  endfunction

  call inputsave()
  let l:prompt = input("Enter desired code prompt: ")
  call inputrestore()
  let l:bufNr = term_start(['llm', '--template', printf("%s", a:template), printf("%s", l:prompt)], {
			  \ 'exit_cb': function('s:exitCb')
			  \ })
endfunction

function! RunCommandAndReplace(template, text)

  function! s:exitCb(job, exitStatus)
	let l:bufNr = ch_getbufnr(job_getchannel(a:job), 'out')
	call setbufvar(l:bufNr, '&buftype', 'nofile')
	call setbufvar(l:bufNr, '&modifiable', 1)

	sleep 200ms

	let choice = confirm('Do you wish to accept these changes?', "&Yes\n&No", 1)

	let content = getline(2, '$')[1:-2]

	if choice == 1
	   " Replace the selected text with the result
	    let @@ = content->join("\n")
	    execute 'b#'
	    normal! gvp

	    " Restore the selection and register settings
	    let &selection = @s
	    let @@ = @r
	    execute 'b#'
	endif

	execute 'bw'
  endfunction

  let l:bufNr = term_start(['llm', '--template', printf("%s", a:template), printf("%s", a:text)], {
			  \ 'exit_cb': function('s:exitCb')
			  \ })
endfunction

function! s:handleContinueExit(job, exitStatus)
  let l:bufNr = ch_getbufnr(job_getchannel(a:job), 'out')
  call setbufvar(l:bufNr, '&buftype', 'nofile')
  call setbufvar(l:bufNr, '&modifiable', 1)

  sleep 200ms

  let choice = confirm('Do you want to discuss further??', "&Yes\n&No", 1)

  let content = getline(2, '$')[1:-2]

  if choice == 1
    execute 'bw'
    call ContinueDiscuss()
  else
   execute 'bw'
   call OpenLastChatInBuffer()
  endif

endfunction


function! ContinueDiscuss()

  call inputsave()
  let l:prompt = input("What more would you like to discuss?: ")
  call inputrestore()

  let l:bufNr = term_start(['llm', 'prompt', "-c", printf("%s", l:prompt)], {
			  \ 'exit_cb': function('s:handleContinueExit')
			  \ })
endfunction

function! RunQuestionAndDiscuss(template, text)

  function! s:exitCb(job, exitStatus)
	let l:bufNr = ch_getbufnr(job_getchannel(a:job), 'out')
	call setbufvar(l:bufNr, '&buftype', 'nofile')
	call setbufvar(l:bufNr, '&modifiable', 1)

	sleep 200ms

	let choice = confirm('Do you want to discuss further??', "&Yes\n&No", 1)

	let content = getline(2, '$')[1:-2]

	if choice == 1
	  execute 'bw'
	   call ContinueDiscuss()
	 else
           execute 'bw'
	   call OpenLastChatInBuffer()
	endif

  endfunction

  call inputsave()
  let l:prompt = input("What more would you like to discuss?: ")
  call inputrestore()

  let l:content = printf("<context>%s</context> <prompt>%s</prompt>", a:text, l:prompt)

  let l:bufNr = term_start(['llm', '--template', printf("%s", a:template), printf("%s", l:content)], {
			  \ 'exit_cb': function('s:exitCb')
			  \ })
endfunction

function! RunCommandAndDiscuss(template, text)

  function! s:exitCb(job, exitStatus)
	let l:bufNr = ch_getbufnr(job_getchannel(a:job), 'out')
	call setbufvar(l:bufNr, '&buftype', 'nofile')
	call setbufvar(l:bufNr, '&modifiable', 1)

	sleep 200ms

	let choice = confirm('Do you want to discuss further??', "&Yes\n&No", 1)

	let content = getline(2, '$')[1:-2]

	if choice == 1
	  execute 'bw'
	   call ContinueDiscuss()
	 else
           execute 'bw'
	   call OpenLastChatInBuffer()
	endif

  endfunction

  let l:bufNr = term_start(['llm', '--template', printf("%s", a:template), printf("%s", a:text)], {
			  \ 'exit_cb': function('s:exitCb')
			  \ })
endfunction

"
" Define the operator function that will work with text objects
function! s:LLMReplacementOperatorFunction(type)
    " Save the current selection and register settings
    let @s = &selection
    let @r = @@

    " Set selection to inclusive
    set selection=inclusive

    if a:type ==# 'char'
        " For character-wise selection
        silent execute "normal! `[v`]y"
    elseif a:type ==# 'line'
        " For line-wise selection
        silent execute "normal! '[V']y"
    elseif a:type ==# 'block'
        " For block-wise selection
        silent execute "normal! `[\<C-V>`]y"
    else
        return
    endif

    " Get the selected text from the default register
    let selected_text = @@

    " Process the text using MyVimFunction
    let result = RunCommandAndReplace(g:template_prefix . "-implementer", selected_text)

endfunction

" Define the operator function that will work with text objects
function! s:LLMRefactorOperatorFunction(type)
    " Save the current selection and register settings
    let @s = &selection
    let @r = @@

    " Set selection to inclusive
    set selection=inclusive

    if a:type ==# 'char'
        " For character-wise selection
        silent execute "normal! `[v`]y"
    elseif a:type ==# 'line'
        " For line-wise selection
        silent execute "normal! '[V']y"
    elseif a:type ==# 'block'
        " For block-wise selection
        silent execute "normal! `[\<C-V>`]y"
    else
        return
    endif

    " Get the selected text from the default register
    let selected_text = @@

    " Process the text using MyVimFunction
    let result = RunCommandAndReplace(g:template_prefix . "-refactor", selected_text)

endfunction

" Define the operator function that will work with text objects
function! s:LLMDiscussionOperatorFunction(type)
    " Save the current selection and register settings
    let @s = &selection
    let @r = @@

    " Set selection to inclusive
    set selection=inclusive

    if a:type ==# 'char'
        " For character-wise selection
        silent execute "normal! `[v`]y"
    elseif a:type ==# 'line'
        " For line-wise selection
        silent execute "normal! '[V']y"
    elseif a:type ==# 'block'
        " For block-wise selection
        silent execute "normal! `[\<C-V>`]y"
    else
        return
    endif

    " Get the selected text from the default register
    let selected_text = @@

    " Process the text using MyVimFunction
    let result = RunCommandAndDiscuss(g:template_prefix . "-discuss", selected_text)

endfunction

" Define the operator function that will work with text objects
function! s:LLMExplainOperatorFunction(type)
    " Save the current selection and register settings
    let @s = &selection
    let @r = @@

    " Set selection to inclusive
    set selection=inclusive

    if a:type ==# 'char'
        " For character-wise selection
        silent execute "normal! `[v`]y"
    elseif a:type ==# 'line'
        " For line-wise selection
        silent execute "normal! '[V']y"
    elseif a:type ==# 'block'
        " For block-wise selection
        silent execute "normal! `[\<C-V>`]y"
    else
        return
    endif

    " Get the selected text from the default register
    let selected_text = @@

    " Process the text using MyVimFunction
    let result = RunCommandAndDiscuss(g:template_prefix . "-explain", selected_text)

endfunction

" Define the operator function that will work with text objects
function! s:LLMCritiqueOperatorFunction(type)
    " Save the current selection and register settings
    let @s = &selection
    let @r = @@

    " Set selection to inclusive
    set selection=inclusive

    if a:type ==# 'char'
        " For character-wise selection
        silent execute "normal! `[v`]y"
    elseif a:type ==# 'line'
        " For line-wise selection
        silent execute "normal! '[V']y"
    elseif a:type ==# 'block'
        " For block-wise selection
        silent execute "normal! `[\<C-V>`]y"
    else
        return
    endif

    " Get the selected text from the default register
    let selected_text = @@

    " Process the text using MyVimFunction
    let result = RunCommandAndDiscuss(g:template_prefix . "-critique", selected_text)

endfunction

function! s:LLMQuestionOperatorFunction(type)
    " Save the current selection and register settings
    let @s = &selection
    let @r = @@

    " Set selection to inclusive
    set selection=inclusive

    if a:type ==# 'char'
        " For character-wise selection
        silent execute "normal! `[v`]y"
    elseif a:type ==# 'line'
        " For line-wise selection
        silent execute "normal! '[V']y"
    elseif a:type ==# 'block'
        " For block-wise selection
        silent execute "normal! `[\<C-V>`]y"
    else
        return
    endif

    " Get the selected text from the default register
    let selected_text = @@

    " Process the text using MyVimFunction
    let result = RunQuestionAndDiscuss("gen-question", selected_text)

endfunction


nnoremap <Plug>LLMReplacementOperator :set opfunc=<SID>LLMReplacementOperatorFunction<CR>g@
nnoremap <Plug>LLMRefactorOperator :set opfunc=<SID>LLMRefactorOperatorFunction<CR>g@
nnoremap <Plug>LLMDiscussionOperator :set opfunc=<SID>LLMDiscussionOperatorFunction<CR>g@
nnoremap <Plug>LLMExplainOperator :set opfunc=<SID>LLMExplainOperatorFunction<CR>g@
nnoremap <Plug>LLMCritiqueOperator :set opfunc=<SID>LLMCritiqueOperatorFunction<CR>g@
nnoremap <Plug>LLMQuestionOperator :set opfunc=<SID>LLMQuestionOperatorFunction<CR>g@

" Leader llm implement
nmap <leader>li <Plug>LLMReplacementOperator
vmap <leader>li <Plug>LLMReplacementOperator

nmap <leader>lr <Plug>LLMRefactorOperator
vmap <leader>lr <Plug>LLMRefactorOperator

" Leader llm discuss
nmap <leader>ld <Plug>LLMDiscussionOperator
vmap <leader>ld <Plug>LLMDiscussionOperator

" Leader llm explain
nmap <leader>le <Plug>LLMExplainOperator
vmap <leader>le <Plug>LLMExplainOperator

" Leader llm explain
nmap <leader>lc <Plug>LLMCritiqueOperator
vmap <leader>lc <Plug>LLMCritiqueOperator

" Leader llm question
nmap <leader>lq <Plug>LLMQuestionOperator
vmap <leader>lq <Plug>LLMQuestionOperator

" Leader llm write
nmap <leader>lw :call RunCommandAndPaste(g:template_prefix . "-code-gen")<CR>

" Leader llm view
nmap <leader>lv :call OpenLastChatInBuffer()<CR>

let g:loaded_llm = 1
