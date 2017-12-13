-module(emailcruncher).
-export([start/0]).
-export([emailExtractor/1]).
-export([resultDisplayer/1]).



process_file(Filename, Email_pid) ->
    case file:read_file(Filename) of
    	{ok, Binary_text} ->
    	    io:fwrite("Parsing ~s~n", [Filename]),
            Bulk_text = binary_to_list(Binary_text),
            Email_pid ! {bulktext, Bulk_text};
        {error, _Reason} ->
            io:fwrite("Error reading ~s~n", [Filename])
    end.



prompt(Email_pid) ->
    {ok, [Filename]} = io:fread("> ", "~s"),

    if 
    	Filename /= "quit" ->
    	    process_file(Filename, Email_pid),
    	    prompt(Email_pid);
    	true ->
    	    io:fwrite("Quitting...~n"),
    	    false
    end.



emailExtractor(Result_pid) ->
    receive
    	{bulktext, Msg} ->
    	    Result = re:run(Msg, 
    	    	"[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", [global]),
    	    case Result of 
    	    	{match, Matches} ->
    	    	    Result_pid ! {num_matches, length(Matches)};
    	    	nomatch ->
    	    	    Result_pid ! {num_matches, 0}
    	    end;
    	quit ->
    	    exit(normal)
    end,
    emailExtractor(Result_pid).



resultDisplayer(Start) ->
    receive
     	{num_matches, Num_matches} ->
     	    Total = Num_matches + Start,
     	    io:fwrite("Received ~w more emails, now ~w in total...~n", 
     	    	[Num_matches, Total]);
     	quit ->
     	    Total = 0,
     	    exit(normal)
    end,
    resultDisplayer(Total).



start() ->
    Result_pid = spawn(emailcruncher, resultDisplayer, [0]),
    Email_pid = spawn(emailcruncher, emailExtractor, [Result_pid]),
    prompt(Email_pid),
    Email_pid ! quit,
    Result_pid ! quit.
