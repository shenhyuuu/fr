# TuFox Prolog Adventure

This project implements the **《兔狐绝杀》** inspired text adventure in SWI-Prolog, including a planner-driven detective rabbit adversary and PDDL assets. Start the game by running:

```bash
swipl -s tufox.pl -g start
```

The game launches automatically and prints the available commands:
- `move(Room).` move between connected rooms
- `perform(Task).` work on tasks in your current room
- `kill(Target).` eliminate a rabbit when in the same room (3-round cooldown)
- `call_meeting.` trigger discussion and voting when you find a body
- `status.` inspect current round, task progress, and cooldowns
- `wait.` end your turn
- `quit.` exit the session

## Files
- `tufox.pl` – main game logic, knowledge base, AI turns, meeting system, and planner integration.
- `adversary_domain.pddl` – STRIPS domain describing movement and identity inspection for the detective.
- `adversary_problem.pddl` – default problem instance used by the detective plan.

If `pyperplan` is available in your environment, the detective dynamically generates a PDDL problem from the current board (rooms, alive agents, and their coordinates) and asks the external planner for the shortest route that inspects every agent; discovering the player immediately ends the game in a rabbit victory. Otherwise, a built-in fallback plan is executed.
