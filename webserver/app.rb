require 'rubygems'
require 'sinatra'


TESTS_DIR = "tests"
CODE_DIR = "../skeleton"

get '/' do
    html = ""

    Dir.entries(TESTS_DIR).select {|f| !File.directory? f}.each{ |f|
        html += "<div><a href='tests/#{f}'>#{f}</a></div>"
    }

    html

end

get '/run' do
    system "cd #{CODE_DIR} && make test"
end

get '/tests/:date' do
    f = File.join(TESTS_DIR, params[:date])

    if File.exists?(f)
        send_file(f)
    else
        "Test not found"
    end
end

