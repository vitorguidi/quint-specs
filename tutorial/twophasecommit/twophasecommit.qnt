module twophasecommit {

    const NR_FOLLOWERS : int

    type LeaderMessageType = PREPARE | COMMIT | ROLLBACK

    type LeaderMessage = {
        content: LeaderMessageType,
        to: NodeId,
    }

    type FollowerMessageType = OK | ABORT

    type FollowerMessage = {
        content: FollowerMessageType,
        src: NodeId,
    }

    type NodeId = int

    type Message = Leader(LeaderMessage) | Follower(FollowerMessage)

    type Wire = Set[Message]

    type LeaderState = FROZEN | PREPARING | COMMITING | ABORTING | DONE

    type FollowerState = ABORTED | WAITING | COMMITED | PROCEEDING

    type TransactionManager = {
        state: LeaderState,
        agreed: Set[NodeId],
    }

    type ResourceManager = {
        id: NodeId,
        state: FollowerState,
    }

    var followers : Set[ResourceManager]

    var tm: TransactionManager

    var messages: Wire

    action init = all {
        tm' = { state: FROZEN, agreed: Set() },
        followers' = 1.to(NR_FOLLOWERS).map(x => { id: x, state: WAITING }),
        messages' = Set(),
    }

    action tm_prepare = all {
        tm.state == FROZEN,
        tm' = { state: PREPARING, agreed: tm.agreed },
        val new_messages = 1.to(NR_FOLLOWERS).map(x => Leader({content:  PREPARE, to: x}))
        messages' = messages.union(new_messages),
        followers' = followers,
    }

    action handle_prepare(id, content) = all {
        followers.exists(f => f.id == id and f.state == WAITING),
        any {
            all {
                followers' = followers.map(x => if (x.id == id) ({ id: x.id, state: PROCEEDING }) else x),
                tm' = tm,
                val new_msg = Follower({content: OK, src: id})
                messages' = messages.union(Set(new_msg))
            },
            all {
                followers' = followers.map(x => if (x.id == id) ({ id: x.id, state: ABORTED }) else x),
                tm' = tm,
                val new_msg = Follower({content: ABORT, src: id})
                messages' = messages.union(Set(new_msg))
            }
        },
    }

    action handle_commit(id, content) = all {
        followers.exists(f => f.id == id and f.state == PROCEEDING),
        followers' = followers.map(x => if (x.id == id) ({ id: x.id, state: COMMITED }) else x),
        tm' = tm,
        messages' = messages,
    }

    action handle_rollback(id, content) = all {
        followers.exists(f => f.id == id and (f.state == PROCEEDING or f.state == WAITING)),
        followers' = followers.map(x => if (x.id == id) ({ id: x.id, state: ABORTED }) else x),
        tm' = tm,
        messages' = messages,
    }

    action rm_receive = all {
        nondet message = messages.oneOf()
        matchVariant(message,
            "Leader", (lm) => matchVariant(lm.content,
                "PREPARE", (_) => handle_prepare(lm.to, lm.content),
                "COMMIT", (_) => handle_commit(lm.to, lm.content),
                "ROLLBACK", (_) => handle_rollback(lm.to, lm.content),
            ),
            "Follower", (_) => all { tm' = tm, followers' = followers, messages' = messages }
        )
    }

    action handle_ok(src) = all {
        tm.state == PREPARING,
        not(tm.agreed.contains(src)),
        followers' = followers,
        val new_agreed = tm.agreed.union(Set(src))
        any {
            all {
                new_agreed.size() == NR_FOLLOWERS,
                tm' = { state: COMMITING, agreed: new_agreed },
                val new_messages = 1.to(NR_FOLLOWERS).map(x => Leader({content: COMMIT, to: x}))
                messages' = messages.union(new_messages),
            },
            all {
                new_agreed.size() != NR_FOLLOWERS,
                tm' = { state: tm.state, agreed: new_agreed },
                messages' = messages,
            }
        }
    }

    action handle_abort(src) = all {
        tm.state == PREPARING,
        tm' = { state: ABORTING, agreed: tm.agreed },
        followers' = followers,
        val new_messages = 1.to(NR_FOLLOWERS).map(x => Leader({content: ROLLBACK, to: x}))
        messages' = messages.union(new_messages)
    }

    action tm_receive = all {
        nondet message = messages.oneOf()
        matchVariant(message,
            "Follower", (fm) => matchVariant(fm.content,
                "OK", (_) => handle_ok(fm.src),
                "ABORT", (_) => handle_abort(fm.src),
            ),
            "Leader", (_) => all { tm' = tm, followers' = followers, messages' = messages }
        )
    }

    action step = any {
        tm_prepare,
        tm_receive,
        rm_receive,
    }

    val no_disagreement = not(
        followers.exists(f => f.state == COMMITED) and
        followers.exists(f => f.state == ABORTED)
    )

}

module twophasecommit3 {
    import twophasecommit(NR_FOLLOWERS=3).*
}