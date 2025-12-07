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
- `tufox.pl` – main game logic, knowledge base, AI turns, meeting system, and planner integration. Meetings now use a planning-based “反事实计划投票制” that compares whether suspects can explain recent rounds when acting as the lone fox versus an innocent rabbit, with detective evidence nudging costs rather than hard reveals.
- `adversary_domain.pddl` – STRIPS domain describing movement and identity inspection for the detective.
- `adversary_problem.pddl` – default problem instance used by the detective plan.

If `pyperplan` is available in your environment, the detective attempts to build a plan from the PDDL files. Otherwise, a built-in fallback plan is executed.
