<?php

echo "<tr id='comment-$comment->comment_ID' class='$the_comment_class'>";

// Not looking at all comments.
                if ( $comment_status && 'all' != $comment_status ) {
                        if ( 'approved' === $the_comment_status ) {
                                $actions['unapprove'] = "<a href='$unapprove_url' data-wp-lists='delete:the-comment-list:comment-$comment->comment_ID:e7e7d3:action=dim-comment&amp;new=unapproved' class='vim-u vim-destructive' aria-label='" . esc_attr__( 'Unapprove this comment' ) . "'>" . __( 'Unapprove' ) . '</a>';
                        } elseif ( 'unapproved' === $the_comment_status ) {
                                $actions['approve'] = "<a href='$approve_url' data-wp-lists='delete:the-comment-list:comment-$comment->comment_ID:e7e7d3:action=dim-comment&amp;new=approved' class='vim-a vim-destructive' aria-label='" . esc_attr__( 'Approve this comment' ) . "'>" . __( 'Approve' ) . '</a>';
                        }
                }

