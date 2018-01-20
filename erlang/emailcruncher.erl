-module(emailcruncher).
-export([start/0]).
-export([emailExtractor/1]).
-export([resultDisplayer/1]).



process_file(Filename, Email_pid) ->
    % helper function that reads a given file,
    % and sends it to Email_pid
    case file:read_file(Filename) of
    	{ok, Binary_text} ->
    	    io:fwrite("Parsing ~s~n", [Filename]),
          Bulk_text = binary_to_list(Binary_text),
          Email_pid ! {bulktext, Bulk_text};
        {error, _Reason} ->
          io:fwrite("Error reading ~s~n", [Filename])
    end.



prompt(Email_pid) ->
    % prompt. Enter the path to a file to be processed
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
    receive  % receive a single message
    	{bulktext, Msg} ->
    	    Result = re:run(Msg,
    	    	"[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", [global]),
    	    case Result of
    	    	{match, Matches} ->
    	    	    Result_pid ! {num_matches, length(Matches)};
                    % send number of new emails to resultDisplayer
    	    	nomatch ->
    	    	    Result_pid ! {num_matches, 0}
    	    end;
    	quit ->
    	    exit(normal)
    end,
    emailExtractor(Result_pid).  % use recursion to process the next message



resultDisplayer(Start) ->
    receive
     	{num_matches, Num_matches} ->
     	    Total = Num_matches + Start,
     	    io:fwrite("Received ~w more emails, now ~w in total...~n",
     	    	[Num_matches, Total]);
            % print the new running total to standard output
     	quit ->
     	    Total = 0,
     	    exit(normal)
    end,
    resultDisplayer(Total).



start() ->
    % spawn the resultDisplayer and emailExtractor micrservices
    Result_pid = spawn(emailcruncher, resultDisplayer, [0]),
    Email_pid = spawn(emailcruncher, emailExtractor, [Result_pid]),
    prompt(Email_pid),  % run the prompt
    Email_pid ! quit,  % kill the microservices once the prompt has finished
    Result_pid ! quit.
