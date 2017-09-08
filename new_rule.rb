require 'YAML'
meta = YAML.load_file('rules/meta')
meta['last_id'] += 1
print 'Name: '
name = gets.chop
print 'Power: '
power = gets.to_f
print 'Proposal ID: '
prop_id = gets.to_i
print 'Proposal Title: '
prop_title = gets.chop
print 'Proposal Authors: '
prop_authors = gets.chop
rule = {
	'name' => name,
	'id' => meta['last_id'],
	'rev' => 0,
	'power' => power,
	'text' => `pbpaste`.gsub(/^  /, ''),
	'history' => ["Created by Proposal #{prop_id} \"#{prop_title}\" (#{prop_authors}), DATE_HERE"],
	'annotations' => []
}
File.open("rules/#{name}", 'w') {|file| YAML.dump(rule, file)}
File.open("rules/meta", 'w') {|file| YAML.dump(meta, file)}
`subl 'rules/#{name}'`
