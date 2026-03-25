# Required data-testid Attributes for E2E Testing

These need to be added to Flux components for reliable E2E test selectors.
Until added, tests use text-based or aria selectors as fallbacks.

| Component | Selector | File |
|-----------|----------|------|
| Join call button | `data-testid="join-call"` | SidebarHeader.vue or CallControls |
| Leave call button | `data-testid="leave-call"` | MainCallControls.vue |
| Call topology indicator | `data-testid="call-indicator"` | SfuIndicator.vue |
| Participant count | `data-testid="participant-count"` | CallWindow.vue |
| Video grid | `data-testid="video-grid"` | VideoGrid.vue |
| Video tile | `data-testid="video-tile"` | VideoGrid.vue child |
| Community sidebar item | `data-testid="community-item"` | Sidebar community list |
| App home (post-auth) | `data-testid="app-home"` | Main app view |
| Connect button | `data-testid="connect-button"` | ad4m-connect dialog |
| Security code input | `data-testid="security-code-input"` | ad4m-connect dialog |
| Confirm code button | `data-testid="confirm-code-button"` | ad4m-connect dialog |
| Mute/unmute audio | `data-testid="toggle-audio"` | CallControls |
| Mute/unmute video | `data-testid="toggle-video"` | CallControls |
| Screen share button | `data-testid="screen-share"` | CallControls |
| SFU mode indicator text | `data-testid="sfu-mode-text"` | SfuIndicator.vue |
