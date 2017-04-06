#!/usr/bin/env ruby
# Copied from https://gist.github.com/jgrau/2b0920e7a30925ea6fbefba4b157c9b8

# Makes accessing the latest exitstatus a bit easier
require 'English'

# Updates all the outdated gems one by one and commits the changes
# for every gem update. This should make it a lot easier to revert
# a gem update.
class UpdateAllGems
  def self.run
    new.update_all
  end

  TEST_CMD = ENV['TEST_CMD'] || 'make build'.freeze

  attr_reader :outdated_gems

  def initialize
    @outdated_gems = fetch_outdated_gems
  end

  def update_all
    outdated_gems.each do |gem_name|
      update_gem(gem_name)
      run_tests(gem_name)
      commit_results(gem_name)
    end
  end

  private

  def fetch_outdated_gems
    puts 'Fetching outdated gems with `bundle outdated`'
    bundler_result = `bundle outdated`

    exit 0 if last_command_success_finished_successfully?

    if last_command_exitstatus != 1
      raise "bundler exited with unexpected results: #{bundler_result}"
    end

    puts 'Outdated gems:'
    puts bundler_result

    extract_gem_names(bundler_result)
  end

  # Example `bundle outdated` output
  # ```
  # Outdated gems included in the bundle:
  #   * active_scheduler (newest 0.3.0, installed 0.0.3) in group "default"
  #   * minitest-reporters (newest 1.1.11, installed 1.1.4) in group "test"
  #   * rails-erd (newest 1.5.0, installed 1.4.4) in group "development"
  #   * hashdiff (newest 0.3.0, installed 0.2.3)
  #   * ice_nine (newest 0.11.2, installed 0.11.1)
  # ```
  def extract_gem_names(text)
    text.each_line.map do |line|
      line.strip!

      # Make sure we only grab lines that are actual outdated gems
      # See the example output below
      next unless line.start_with?('* ')

      # Make sure we only outdate gems that are in our Gemfile
      # See the example output below
      next unless line.include?('in group')

      line.split(' ')[1]
    end.compact
  end

  def update_gem(gem_name)
    puts "Updating #{gem_name} with `bundle update #{gem_name}`"
    bundler_result = `bundle update #{gem_name}`

    unless last_command_success_finished_successfully?
      raise "Failed to update gem #{gem_name}: #{bundler_result}"
    end

    true
  end

  def run_tests(gem_name)
    puts 'Running tests with `#{TEST_CMD}`'
    test_result = `#{TEST_CMD}`

    unless last_command_success_finished_successfully?
      raise "Tests fail after updating #{gem_name}: #{test_result}"
    end

    true
  end

  def commit_results(gem_name)
    puts "Committing updated #{gem_name}"
    git_result = `git commit -am "Updated #{gem_name}"`

    unless last_command_success_finished_successfully?
      raise "Failed to commit results to git: #{git_result}"
    end
  end

  def last_command_exitstatus
    $CHILD_STATUS.exitstatus
  end

  def last_command_success_finished_successfully?
    last_command_exitstatus.zero?
  end
end

UpdateAllGems.run
