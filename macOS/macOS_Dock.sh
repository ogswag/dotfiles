# Set Dock icon size to 48px
echo 'Setting Dock icon size to 48px'; sleep 0.025;
defaults write com.apple.dock tilesize -int 48;

# Wipe all pinned icons from Dock
echo 'Deleting all pinned icons from Dock'; sleep 0.025;
defaults write com.apple.dock persistent-apps -array;

# Change minimize/maximize window effect
defaults write com.apple.dock mineffect -string "scale";

# Move Dock to the left
echo 'Moving Dock to the left'; sleep 0.025;
defaults write com.apple.dock orientation left

# Hide the Dock
echo 'Hiding the Dock'; sleep 0.025;
defaults write com.apple.dock autohide -bool true;

# Remove hiding/showing delay
echo 'Removing Dock hiding/showing delay'; sleep 0.025;
defaults write com.apple.Dock autohide-delay -float 0.005;

# Turn off icon bouncing
echo 'Turning off icon bouncing in the Dock'; sleep 0.025;
defaults write com.apple.dock no-bouncing -bool TRUE;
# Don’t animate opening applications from the Dock
defaults write com.apple.dock launchanim -bool false

# Speed up Mission Control animations
defaults write com.apple.dock expose-animation-duration -float 0.025

# Highlight icons when hovering in grid mode
echo "Turning on icon highlight in grid mode"; sleep 0.025;
defaults write com.apple.dock mouse-over-hilite-stack -boolean yes;

# Make Dock icons of hidden applications translucent
defaults write com.apple.dock showhidden -bool true

# Don’t show recent applications in Dock
echo "Don't show recent applications in Dock"; sleep 0.025;
defaults write com.apple.dock show-recents -bool false

# Add /Applications folder
echo "Adding /Applications folder to Dock"; sleep 0.025;
defaults write com.apple.dock persistent-others -array-add "\
<dict>\
  <key>tile-data</key>\
    <dict>\
      <key>file-data</key>\
        <dict>\
          <key>_CFURLString</key> <string>file:///Applications/</string>\
          <key>_CFURLStringType</key> <integer>15</integer>\
        </dict>\

      <key>file-type</key> <integer>2</integer>\
      <key>showas</key> <integer>1</integer>\
    </dict>\

  <key>tile-type</key> <string>directory-tile</string>\
</dict>";

echo 'Restarting Dock'; sleep 0.025;
killall Dock;
