ignore %r{^tests/Doc/}, /elm-stuff/

guard 'kjell', cmd: "elm-doc-test && elm-test", :all_on_start => true do
  watch(%r{src/.+\.elm$})
  watch(%r{tests/.+\.elm$})
end

guard 'kjell', cmd: "elm-make src/Grizzled.elm --output=grizzled-elm.js", :all_on_start => true do
  watch(%r{src/.+\.elm$})
end

guard 'kjell', cmd: "elm-css src/Stylesheets.elm", :all_on_start => true do
  watch(%r{src/Style.elm})
end

guard 'livereload' do
  watch(%r{grizzled-elm.js})
  watch(%r{styles.css})
end
