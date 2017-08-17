require_relative 'common.rb'
require 'yaml'
require 'ostruct'

index = YAML.load_file('rules/index')
rule_groups = index.map do |name, rule_names|
  {name: name, rules: rule_names.map do |rn|
    YAML.load_file("rules/#{rn}")
  end}
end

rule_groups.each do |rg|
	rg[:rules].each do |r|
		r['history'] = get_history(OpenStruct.new(r))
		r['rev'] = get_rev(OpenStruct.new(r))
		File.open("rules/#{r['name']}", 'w') do |file|
			YAML.dump r.to_h, file
		end
	end
end
