# mcclim-render-stack

McCLIM backend using render-stack (Flutter Impeller + SDL3).

## Purpose

Provides a modern GPU-accelerated backend for McCLIM:
- Port: SDL3-based windowing and event handling
- Medium: Impeller-based drawing operations
- Graft: Display/screen abstraction
- Frame manager: Window lifecycle management

## Status

**Early development** - skeleton stubs only.

## Dependencies

- mcclim (the main McCLIM system)
- render-stack (core engine)
- flutter-render-stack (Impeller + Flow)
- render-stack-sdl3 (SDL3 windowing)

## Usage

```lisp
(ql:quickload :mcclim-render-stack)

;; Use as the default backend
(setf clim:*default-server-path* '(:render-stack))

;; Or explicitly
(clim:run-frame-top-level
  (clim:make-application-frame 'my-app)
  :server-path '(:render-stack))
```

## Architecture

```
┌─────────────────────────────────────────┐
│              McCLIM                      │
│  (application frames, presentation      │
│   types, commands, gadgets)             │
├─────────────────────────────────────────┤
│        mcclim-render-stack              │
│  (port, medium, graft, frame-manager)   │
├─────────────────────────────────────────┤
│           render-stack                   │
│  (frame clock, pipeline, animation)     │
├───────────────────┬─────────────────────┤
│ flutter-render-   │  render-stack-sdl3  │
│ stack             │                     │
└───────────────────┴─────────────────────┘
```

## Related Projects

- [McCLIM](https://github.com/McCLIM/McCLIM) - Common Lisp Interface Manager
- [render-stack](https://github.com/jolby/render-stack) - Core rendering engine
- [flutter-render-stack](https://github.com/jolby/flutter-render-stack) - Impeller + Flow wrappers
- [render-stack-sdl3](https://github.com/jolby/render-stack-sdl3) - SDL3 wrappers

## License

MIT
