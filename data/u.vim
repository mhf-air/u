" Author: Daniel Schemala <istjanichtzufassen@gmail.com>,
" Ivan Petkov <ivanppetkov@gmail.com>
" Description: uc invoked by u for u files

call ale#Set('u_cargo_use_check', 1)
call ale#Set('u_cargo_check_all_targets', 0)
call ale#Set('u_cargo_check_examples', 0)
call ale#Set('u_cargo_check_tests', 0)
call ale#Set('u_cargo_avoid_whole_workspace', 1)
call ale#Set('u_cargo_default_feature_behavior', 'default')
call ale#Set('u_cargo_include_features', '')
call ale#Set('u_cargo_use_clippy', 0)
call ale#Set('u_cargo_clippy_options', '')
call ale#Set('u_cargo_target_dir', '')

function! ale_linters#u#u#GetCargoExecutable(bufnr) abort
	if ale#path#FindNearestFile(a:bufnr, 'u.toml') isnot# ''
		return 'u-cargo-lint'
	else
		" if there is no u.toml file, we don't use u even if it exists,
		" so we return '', because executable('') apparently always fails
		return ''
	endif
endfunction

function! ale_linters#u#u#GetCwd(buffer) abort
	if ale#Var(a:buffer, 'u_cargo_avoid_whole_workspace')
		let l:nearest_cargo = ale#path#FindNearestFile(a:buffer, 'u.toml')
		let l:nearest_cargo_dir = fnamemodify(l:nearest_cargo, ':h')

		if l:nearest_cargo_dir isnot# '.'
			return l:nearest_cargo_dir
		endif
	endif

	return ''
endfunction

function! ale_linters#u#u#GetCommand(buffer, version) abort
	let l:use_check = ale#Var(a:buffer, 'u_cargo_use_check')
	\   && ale#semver#GTE(a:version, [0, 17, 0])
	let l:use_all_targets = ale#Var(a:buffer, 'u_cargo_check_all_targets')
	\   && ale#semver#GTE(a:version, [0, 22, 0])
	let l:use_examples = ale#Var(a:buffer, 'u_cargo_check_examples')
	\   && ale#semver#GTE(a:version, [0, 22, 0])
	let l:use_tests = ale#Var(a:buffer, 'u_cargo_check_tests')
	\   && ale#semver#GTE(a:version, [0, 22, 0])
	let l:target_dir = ale#Var(a:buffer, 'u_cargo_target_dir')
	let l:use_target_dir = !empty(l:target_dir)
	\   && ale#semver#GTE(a:version, [0, 17, 0])

	let l:include_features = ale#Var(a:buffer, 'u_cargo_include_features')

	if !empty(l:include_features)
		let l:include_features = ' --features ' . ale#Escape(l:include_features)
	endif

	let l:default_feature_behavior = ale#Var(a:buffer, 'u_cargo_default_feature_behavior')

	if l:default_feature_behavior is# 'all'
		let l:include_features = ''
		let l:default_feature = ' --all-features'
	elseif l:default_feature_behavior is# 'none'
		let l:default_feature = ' --no-default-features'
	else
		let l:default_feature = ''
	endif

	let l:subcommand = l:use_check ? 'check' : 'build'
	let l:clippy_options = ''

	if ale#Var(a:buffer, 'u_cargo_use_clippy')
		let l:subcommand = 'clippy'
		let l:clippy_options = ale#Var(a:buffer, 'u_cargo_clippy_options')

		if l:clippy_options =~# '^-- '
			let l:clippy_options = join(split(l:clippy_options, '-- '))
		endif

		if l:clippy_options isnot# ''
			let l:clippy_options = ' -- ' . l:clippy_options
		endif
	endif

	return 'u-cargo-lint '
	\   . l:subcommand
	\   . (l:use_all_targets ? ' --all-targets' : '')
	\   . (l:use_examples ? ' --examples' : '')
	\   . (l:use_tests ? ' --tests' : '')
	\   . (l:use_target_dir ? (' --target-dir ' . ale#Escape(l:target_dir)) : '')
	\   . ' --frozen --message-format=json -q'
	\   . l:default_feature
	\   . l:include_features
	\   . l:clippy_options
endfunction

call ale#linter#Define('u', {
\   'name': 'u',
\   'executable': function('ale_linters#u#u#GetCargoExecutable'),
\   'cwd': function('ale_linters#u#u#GetCwd'),
\   'command': {buffer -> ale#semver#RunWithVersionCheck(
\       buffer,
\       ale_linters#u#u#GetCargoExecutable(buffer),
\       '%e --version',
\       function('ale_linters#u#u#GetCommand'),
\   )},
\   'callback': 'ale#handlers#rust#HandleRustErrors',
\   'output_stream': 'both',
\   'lint_file': 1,
\})
