# 🔤 Word Guessing Game (Haskell)

![Haskell](https://img.shields.io/badge/Haskell-Stack/Cabal-5e5086?logo=haskell&logoColor=white)
![Platform](https://img.shields.io/badge/Platform-Console-lightgrey)
![Data](https://img.shields.io/badge/Data-CSV%20(words.csv)-green)
![Category](https://img.shields.io/badge/Category-Word%20Game-orange)

## 📌 Introduction
A text-based word-guessing game written in **Haskell**. Players choose a difficulty, guess a hidden word within limited attempts, and can request progressive hints. Scores and player stats are tracked across rounds to encourage improvement.

---

## 🚀 Features
- **Three difficulties** — Easy / Medium / Hard (separate pools, scaled scoring).
- **Fair guessing** — case-insensitive matches, trimmed input, validation.
- **Progressive hints** — category → first letter → internal letter.
- **Attempts & flow** — 5 attempts per word; continue, resume, or return to menu.
- **Scoreboard & stats** — total score, guesses, correct answers, hints used.
- **CSV word list** — easy to extend/edit via `words.csv`.

---

## 🕹 Gameplay Overview
1. **Main Menu** → View instructions, start/resume, view score & stats, reset, or exit.  
2. **Choose Difficulty** → filters the word pool.  
3. **Guess Loop** → type a guess or `h` for a hint (limited).  
4. **Win/Lose** → score updates by difficulty.  
5. **Continue** → play again or return to menu.

**Attempts:** 5 per word • **Hints:** limited & tracked • **Scoring:** +1 / +2 / +3 (Easy / Medium / Hard)

---

## 🗂 Data Format (`words.csv`)
Each line:

- `difficulty` is one of: `easy`, `medium`, `hard` (case-insensitive).
- Invalid lines are skipped safely.

---

## 🧠 Design Notes
- **Types**
  - `WordData` — word + difficulty + category  
  - `PlayerStats` — guesses, correct answers, hints used (Semigroup/Monoid)  
  - `Score` — newtype around `Sum Int`  
  - `Difficulty` — ADT (`Easy | Medium | Hard`) for safe parsing & scoring
- **Safety**
  - No partial functions (`head`, `(!!)` guarded).  
  - Safe CSV parsing and total input handling.  
- **UX**
  - Clear prompts, consistent icons (✅/❌/💡), resume vs. new game logic.

---

## 🔚 Conclusion
A safe, extensible, and fun **console word game** that showcases practical Haskell: clean types, total functions, robust parsing, and a friendly user loop — powered by a simple CSV word pool.

