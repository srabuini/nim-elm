use Rack::Static,
    urls: ['/img', '/js', '/css'],
    root: 'public',
    index: 'index.html'

run Rack::File.new('public')
