<?php
function stream_notification_callback($notification_code, $severity, $message, $message_code, $bytes_transferred, $bytes_max) {

    switch($notification_code) {
        case STREAM_NOTIFY_RESOLVE:
        case STREAM_NOTIFY_AUTH_REQUIRED:
        case STREAM_NOTIFY_COMPLETED:
        case STREAM_NOTIFY_FAILURE:
        case STREAM_NOTIFY_AUTH_RESULT:
            var_dump($notification_code, $severity, $message, $message_code, $bytes_transferred, $bytes_max);
            /* Ignore */
            break;

        case STREAM_NOTIFY_REDIRECTED:
            echo "Being redirected to: ", $message;
            break;

        case STREAM_NOTIFY_CONNECT:
            echo "Connected...";
            break;

        case STREAM_NOTIFY_FILE_SIZE_IS:
            echo "Got the filesize: ", $bytes_max;
            break;

        case STREAM_NOTIFY_MIME_TYPE_IS:
            echo "Found the mime-type: ", $message;
            break;

        case STREAM_NOTIFY_PROGRESS:
            echo "Made some progress, downloaded ", $bytes_transferred, " so far";
            break;
    }
    echo "\n";
}

$ctx = stream_context_create();
stream_context_set_params($ctx, array("notification" => "stream_notification_callback"));

file_get_contents("http://php.net/contact", false, $ctx);

