<?

$heredoc = <<<HERE

This is a heredoc! :-)
just as it should be

HERE;

print $heredoc;

$heredoc = <<<END

And another here doc, with
a tramp...
HERE;

END;

print $heredoc;
