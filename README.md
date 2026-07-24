# gsd-yesod

An example http/json web-service written in Haskell using Yesod + Persistent.

Domain Objects:

- Milestone: A long-term goal (spans multiple sprints).
  - Can have a start and completion timestamp.
  - Can be linked to any number of stories (many-to-many relation).
- Story: A single feature (implemented in one sprint).
  - Effort is indicated using points.
  - Can have zero or more tasks.
- Task: A step taken to implement a story.
  - Status can be 'Todo' or 'Done' (sum-type allows for expansion).
