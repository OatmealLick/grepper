-module(file_handler).

%% API
-export([handle/1]).

handle(FilePath) ->
  {ok, File} = file:read_file(FilePath),
  text_dispatcher:dispatch(File).