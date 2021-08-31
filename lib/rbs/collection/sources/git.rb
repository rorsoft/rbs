require 'digest/sha2'
require 'open3'

module RBS
  module Collection
    module Sources
      class Git
        METADATA_FILENAME = '.rbs_meta.yaml'

        class CommandError < StandardError; end

        attr_reader :name, :remote, :repo_dir

        def initialize(name:, revision:, remote:, repo_dir:)
          @name = name
          @remote = remote
          @repo_dir = repo_dir || 'gems'

          setup!(revision: revision)
        end

        def has?(config_entry)
          gem_name = config_entry['name']
          gem_repo_dir.join(gem_name).directory?
        end

        def versions(config_entry)
          gem_name = config_entry['name']
          gem_repo_dir.join(gem_name).glob('*/').map { |path| path.basename.to_s }
        end

        def install(dest:, config_entry:, stdout:)
          gem_name = config_entry['name']
          version = config_entry['version'] or raise
          gem_dir = dest.join(gem_name, version)

          if gem_dir.directory?
            if (prev = YAML.load_file(gem_dir.join(METADATA_FILENAME))) == config_entry
              stdout.puts "Using #{format_config_entry(config_entry)}"
            else
              # @type var prev: RBS::Collection::Config::gem_entry
              stdout.puts "Updating to #{format_config_entry(config_entry)} from #{format_config_entry(prev)}"
              FileUtils.remove_entry_secure(gem_dir.to_s)
              _install(dest: dest, config_entry: config_entry)
            end
          else
            stdout.puts "Installing #{format_config_entry(config_entry)}"
            _install(dest: dest, config_entry: config_entry)
          end
        end

        private def _install(dest:, config_entry:)
          gem_name = config_entry['name']
          version = config_entry['version'] or raise
          dest = dest.join(gem_name)
          dest.mkpath
          src = gem_repo_dir.join(gem_name, version)

          FileUtils.cp_r(src, dest)
          dest.join(version, METADATA_FILENAME).write(YAML.dump(config_entry))
        end

        def to_lockfile
          {
            'type' => 'git',
            'name' => name,
            'revision' => resolved_revision,
            'remote' => remote,
            'repo_dir' => repo_dir,
          }
        end

        private def format_config_entry(config_entry)
          name = config_entry['name']
          v = config_entry['version']

          rev = resolved_revision[0..10]
          desc = "#{name}@#{rev}"

          "#{name}:#{v} (#{desc})"
        end

        private def setup!(revision:)
          git_dir.mkpath
          if git_dir.join('.git').directory?
            if need_to_fetch?(revision)
              git 'fetch', 'origin'
            end
          else
            git 'clone', remote, git_dir.to_s
          end

          begin
            git 'checkout', "origin/#{revision}" # for branch name as `revision`
          rescue CommandError
            git 'checkout', revision
          end
        end

        private def need_to_fetch?(revision)
          return true unless revision.match?(/\A[a-f0-9]{40}\z/)

          begin
            git('cat-file', '-e', revision)
            false
          rescue CommandError
            true
          end
        end

        private def git_dir
          @git_dir ||= (
            base = Pathname(ENV['XDG_CACHE_HOME'] || File.expand_path("~/.cache"))
            dir = base.join('rbs', Digest::SHA256.hexdigest(remote))
            dir.mkpath
            dir
          )
        end

        private def gem_repo_dir
          git_dir.join @repo_dir
        end

        private def resolved_revision
          @resolved_revision ||= resolve_revision
        end

        private def resolve_revision
          git('rev-parse', 'HEAD').chomp
        end

        private def git(*cmd)
          sh! 'git', *cmd
        end

        private def sh!(*cmd)
          RBS.logger.debug "$ #{cmd.join(' ')}"
          Open3.capture3(*cmd, chdir: git_dir).then do |out, err, status|
            raise CommandError, "Unexpected status #{status.exitstatus}\n\n#{err}" unless status.success?

            out
          end
        end
      end
    end
  end
end
